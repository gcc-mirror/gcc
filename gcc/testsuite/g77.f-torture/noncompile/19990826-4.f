* Culled from 970528-1.f in Burley's g77 test suite.  Copyright
* status not clear.  Feel free to chop down if the bug is still
* reproducible (see end of test case for how bug shows up in gdb
* run of f771).  No particular reason it should be a noncompile
* case, other than that I didn't want to spend time "fixing" it
* to compile cleanly (with -O0, which works) while making sure the
* ICE remained reproducible.  -- burley 1999-08-26

* Date: Mon, 26 May 1997 13:00:19 +0200 (GMT+0200)
* From: "D. O'Donoghue" <dod@da.saao.ac.za>
* To: Craig Burley <burley@gnu.ai.mit.edu>
* Cc: fortran@gnu.ai.mit.edu
* Subject: Re: g77 problems

	program dophot
	parameter (napple = 4)
        common /window/nwindo,ixwin(50),iywin(50),iboxwin(50),itype(50)
        common/io/luout,ludebg
	common/search/nstot,thresh
	common /fitparms / acc(npmax),alim(npmax),mit,mpar,mfit1,
     +                     mfit2,ind(npmax)
	common /starlist/ starpar(npmax,nsmax), imtype(nsmax),
	1shadow(npmax,nsmax),shaderr(npmax,nsmax),idstr(nsmax)
	common /aperlist/ apple(napple ,nsmax)
	common /parpred / ava(npmax)
	common /unitize / ufactor
	common /undergnd/ nfast, nslow
	common/bzero/ scale,zero
	common /ctimes / chiimp, apertime, filltime, addtime
	common / drfake / needit 
	common /mfit/ psfpar(npmax),starx(nfmax),stary(nfmax),xlim,ylim
	common /vers/ version
 	logical needit,screen,isub,loop,comd,burn,wrtres,fixedxy
	logical fixed,piped,debug,ex,clinfo
	character header*5760,rhead*2880
	character yn*1,version*40,ccd*4,infile*20
	character*30 numf,odir,record*80
	integer*2 instr(8)
	character*800 line
	external pseud0d, pseud2d, pseud4d, pseudmd, shape
C
C	Initialization
	data burn,   fixedxy,fixed,  piped 
     +     /.false.,.false.,.false.,.false./
	data needit,screen,comd,isub
     + /.true.,.false.,.true.,.false. /
	data acc / .01, -.03, -.03, .01, .03, .1, .03 /
	data alim / -1.0e8, 2*-1.0e3, -1.0e8, 3*-1.0e3 /
C
	version = 'DoPHOT Version 1.0 LINUX May 97 '
        debug=.false.
        clinfo=.false.
	line(1:800) = ' '
	odir = ' '
C
C
C	Read default tuneable parameters 
	call tuneup ( nccd, ccd, piped, debug )
	version(33:36) = ccd(1:4)
C
      
        ludebg=6
        if(piped)then
          yn='n'
        else
	  write(*,'(''****************************************'')')
	  write(*,1000) version
	  write(*,'(''****************************************''//)')           
C                                                             
          write(*,'(''Screen output (y/[n])? '',$)')             
	  read(*,1000) yn
        end if
	if(yn.eq.'y'.or.yn.eq.'Y') then
          screen=.true.                
          luout=6
        else
          luout=2
        end if
C
        if(piped)then
          yn='y'
        else
          write(*,'(''Batch mode ([y]/n)? '',$)')
          read(*,1000) yn
        end if
	if(yn.eq.'n'.or.yn.eq.'N') comd = .false.
C                                          
	if(.not.comd) then                                     
          write(*,
     *         '(''Do you want windowing ([y]/n)? '',$)')
          read(*,1000)yn
          iwindo=1
          if(yn.eq.'n'.or.yn.eq.'N')then
            nwindo=0
            iwindo=0
          end if
C
          write(*,
     *       '(''Star classification info (y/[n]) ?'',$)')
          read(*,1000)yn
          clinfo=.false.
          if(yn.eq.'y'.or.yn.eq.'Y')clinfo=.true.
C
	  write(*,
     *        '(''Create a star-subtracted frame (y/[n])? '',$)')
	  read(*,1000) yn                                     
 	  if(yn.eq.'y'.or.yn.eq.'Y') isub = .true.
C               
	  write(*,'(''Apply after-burner (y/[n])? '',$)')
	  read(*,1000) yn
	  if ( yn.eq.'y'.or.yn.eq.'Y' ) burn = .true.
	  wrtres = burn
C
	  write(*,'(''Read from fixed (X,Y) list (y/[n])? '',$)')
	  read(*,1000) yn
	  if ( yn.eq.'y'.or.yn.eq.'Y' ) then
	    fixedxy = .true.
	    fixed = .true.
	    burn = .true.
	    wrtres = .true.
	  endif
	endif         
        iopen=0
C
C       This is the start of the loop over the input files
c
        iframe=0
        open(10,file='timing',status='unknown',access='append')

1	ifit = 0
	iapr = 0
	itmn = 0
	model = 1
	xc = 0.0
	yc = 0.0
	rc = 0.0
	ibr = 0
	ixy = 0
C	
        iframe=iframe+1
        tgetpar=0.0
        tsearch=0.0
        tshape=0.0
        timprove=0.0
C
C	Batch mode ...

	if ( comd ) then
          if(iopen.eq.0)then
            iopen=1
            open(11,file='dophot.bat',status='old',err=995)
          end if
          read(11,1000,end=999)infile
c         now read in the parameter instructions. these are:
c         instr(1) : if 1, specifies uncrowded field, otherwise crowded 
c         instr(2) : if 1, specifies sequential frames of same field
c                          with a window around the stars of interest -
c                          all other objects are ignored
c         instr(3) : if 0, takes cmin from dophot.inp (via tuneup)
c                    if>0, sets cmin=instr(3)
c         instr(4) : if 0, does nothing
c                    if 1, then opens a file called classifications
c                    sets clinfo to .true. and writes out the star
c                    typing info to this file
c         instr(5) : Delete the shd.nnnnnnn file
c         instr(6) : Delete the out.nnnnnnn file
c         instr(7) : Delete the input frame
c         instr(8) : Create a star-subtracted frame 
          read(11,*)instr
          read(11,*)ifit,iapr,tmn,model,xc,yc,rc,ibr,ixy
          nocrwd = instr(1)
          iwindo=instr(2)
          if(iwindo.eq.0)nwindo=0
          itmn=tmn
          if ( instr(3).gt.0 ) cmin=instr(3)
          clinfo=.false.
          if ( instr(4).gt.0 )then
            clinfo=.true.
            open(12,file='classifications',status='unknown')
            ludebg=12
          end if
 	  if ( instr(8).ne.0 ) then
	    isub = .true.            
	  else
	    isub = .false.
	  endif
C
	  if(ibr.ne.0) burn = .true.
	  if(ixy.ne.0) then
	    fixedxy = .true.
	    fixed = .true.
	    burn = .true.
	    goto 20
          endif
          if(iwindo.eq.0)then
            write(6,10)iframe,infile(1:15)
   10       format('  ***** DoPHOT-ing frame ',i4,': ',a)
            if(ludebg.eq.12)write(ludebg,11)iframe,infile(1:15)
   11       format(////'  ',62('*')/
     *                 '  *     DoPHOT-ing frame ',i4,': ',a,
     *                 '                 *'/'  ',62('*'))
          end if
          if(iwindo.eq.1)then
            write(6,12)iframe,infile(1:15)
   12       format('  ***** DoPHOT-ing frame ',i4,': ',a,
     *             '   - Windowed *****')
            if(ludebg.eq.12)write(ludebg,13)iframe,infile(1:15)
   13       format(////'  ',62('*')/
     *                 '  *     DoPHOT-ing frame ',i4,': ',a,
     *                 '   - Windowed    *'/2x,62('*'))
          end if
C
C	Interactive...
	else
	  write(*,'(''Image name: '',$)')
	  read(*,1000) infile
	  if(infile(1:1).eq.' ') goto 999                     
1000	  format(a)                          
          write(*,'(''Crowded field mode ([y]/n) ? '',$)')
          read(*,1000)yn
          nocrwd=0
          if(yn.eq.'n'.or.yn.eq.'N')nocrwd=1
	  if(.not.fixed) then
	    write(*,1001)
1001        format('Sky model ([1]=Plane, 2=Power, 3=Hubble)? ',$)
            read(*,1000)record
            if(record.ne.' ')then
	      read(record,*) model
            else
              model=1
            end if
	  else         
	    burn=.true.
	    goto 20           
	  endif
	endif
C
C       if windowing, open the file and read the window
        if(iwindo.eq.1)then
          inquire(file='windows',exist=ex)
          if(.not.ex)go to 997
          if(iframe.eq.1)open(9,file='windows',status='old')
          nwindo=0
    2     read(9,*,end=3)intype,inx,iny,inbox
          nwindo=nwindo+1
          if(nwindo.gt.50)then
            print *,'too many windows - max = 50'
            stop
          end if
          ixwin(nwindo)=inx
          iywin(nwindo)=iny
          iboxwin(nwindo)=inbox
          itype(nwindo)=intype
          go to 2

    3     rewind 9
          if(screen)print 4,(itype(j),ixwin(j),iywin(j),iboxwin(j),
     *                       j=1,nwindo)
    4     format(' Windows: Type   X    Y   Size'/
     *           (I13,i6,i5,i5))
        end if

	t1 = cputime(0.0)
C
C	Read FITS frame.
	call getfits(1,infile,header,nhead,nfast,nslow,numf,nc,line,ccd)
C
C	Ignore frame if not the correct chip
	if(nc.lt.0) goto 900
C                     
C	Estimate starting PSF parameters.
   15   call getparams(nfast,nslow,gxwid,gywid,skyval,tmin,tmax,
     *                 iframe)
        tgetpar = cputime(t1) + tgetpar
        if(debug)write(ludebg,16)iframe,skyval,gxwid,gywid,tmin,tmax
   16   format(' Getparams on frame ',i4,'  sky ',f6.1,'  gxwid ',f5.1,
     *         '  gywid ',f5.1,'  tmin ',f5.1,'  tmax ',f5.1)
C
C	Initialize
	do j=1,nsmax
	  imtype(j) = 0
	  do i=1,npmax  
	    shadow(i,j)=0.                               
	    shaderr(i,j)=0.
	  enddo
	enddo
C              
	skyguess=skyval
	tfac = 1.0            
C	Use 4.5 X SD as fitting width       
	fitr=fitfac*(gxwid*asprat*gywid)**0.25 + 0.5                      
	i=fitr
	irect(1)=i
	irect(2)=fitr/asprat 
C	Use 4/3 X FitFac X SD as aperture width
	gmax = asprat*gywid
 	if(gxwid.gt.gmax) gmax=gxwid
	aprw = 1.33*fitfac*sqrt(gmax) + 0.5
	i = aprw
	arect(1) = i
	i = aprw/asprat + 0.1
	arect(2) = i          
C                                     
	if(irect(1).gt.50) irect(1)=50
	if(irect(2).gt.50) irect(2)=50  
	if(arect(1).gt.45.) arect(1)=45.
	if(arect(2).gt.45.) arect(2)=45.
C
	if (screen) call htype(line,skyval,.false.,fitr,ngr,ncon)
C
C       Prompt for further information         
	if ( .not.comd ) then
          write(*,1002)
 1002     format(/'The above are the inital parameters DoPHOT'/
     *            'has found. You can change them now or accept'/
     *            'the values in [ ] by pressing enter'/)

          write(*,1004)tmin
 1004     format('Enter Tmin: threshold for star detection',
     *           ' [',f5.1,']  ',$)
          read(*,1000)record
          if(record.ne.' ')read(record,*)tmin

          write(*,1005)cmin
 1005     format('Enter Cmin: threshold for PSF stars',
     *           '      [',f5.1,']  ',$)
          read(*,1000)record
          if(record.ne.' ')read(record,*)cmin

          write(*,1006)
 1006     format('Do you want to fix the aperture mag size ?',
     *           ' (y/[n]) ')
          read(*,1000)record
          if(record.eq.'y'.or.record.eq.'Y')then
            write(*,1007)
 1007       format('Enter the size in pixels: ',$)
            read(*,*)iapr
 	    if(iapr.gt.0) then          
              arect(1)=iapr
              i = iapr/asprat + 0.1
              arect(2)=i
            end if
	  endif                     
C
	  write(*,1008)
 1008     format('Satisfied with other input parameters ? ([y]/n)?',$)
	  read(*,1000) yn        
          if(yn.eq.'n'.or.yn.eq.'N')then
            yn='n'
          else
            yn='y'
          end if
	  if(.not.(yn.eq.'y'.or.yn.eq.'Y') ) call input
	else
	  if ( ifit.ne.0 ) then
	    irect(1)=ifit
	    irect(2)=(ifit/asprat + 0.1)
	  endif              
	  if ( iapr.ne.0 ) then
	    arect(1)=iapr
	    i = iapr/asprat + 0.1
	    arect(2)=i
	  endif                                       
	  if ( itmn.ne.0 ) tmin = itmn
 	  if ( .not.(xc.eq.0.0.and.yc.eq.0.0) ) then
	    xcen = xc
 	    ycen = yc
          endif
	endif          
C
C--------------------------------
C
C
	call setup ( numf,nc,screen,line,skyval,fitr,ngr,ncon,
     +nfast, nslow )
C
C       if the uncrowded field option has been chosen, jump
C       straight to the minimum threshold
C
        if(nocrwd.eq.1)tmax=tmin
C             
C	Adjust tfac so that thresh ends precisely on Tmin.
	if(tmin/tmax .gt. 0.999) then
	  thresh = tmin
	  tfac = 1.          
	else                                                     
	  thresh = tmax
	  xnum = alog10(tmax/tmin)/alog10(2.**tfac)
	  if(xnum.gt.1.5) then
	    xnum = float(nint(xnum))
	  else if(xnum.ge.1) then               
	    xnum = 2.0
	  else             
	    xnum = 1.0             
	  endif                                         
	  tfac = alog10(tmax/tmin)/alog10(2.)/xnum                   
	endif
C                       
C------------------------------------------------------------------------
C                            
C         This is the BIG LOOP which searches the frame for stars
C               with intensities > thresh.                
C        
C-----------------------------------------------------------------------
C                           
	loop = .true.
	nstot = 0
	do while ( loop )   
	  loop = thresh/tmin .ge. 1.01
	  write(luout,1050) thresh
1050	  format(/20('-')/'THRESHOLD: ', f10.3)
	  if(ludebg.eq.12)write(ludebg,1050) thresh
C
C         Fit given model to sky values.
C
          call varipar(nstot, nfast, nslow )            
	  t1 = cputime(0.0)
C               
C         Identifies potential objects in cleaned array IMG
 	  nstar = isearch( pseud2d, nfast, nslow , clinfo)
	  tsearch = cputime(t1) + tsearch
C                                                                   
    	  if ( (nstar .ne. 0).or.(xnum.lt.1.5) ) then
C                                           
C           Performs 7-parameter PSF fit and determines nature of object.
	    t1 = cputime(0.0)
	    call shape(pseud2d,pseud4d,nfast,nslow,clinfo)
	    tshape = cputime(t1) + tshape
C                           
C           Computes average sky values etc from star list
 	    call paravg
  	    t1 = cputime(0.0)
C                                                          
C           Computes 4-parameter fits for all stellar objects using 
C           new average shape parameters.  
  	    call improve(pseud2d,nfast,nslow,clinfo)
	    timprove = cputime(t1) + timprove
	  end if                         
C
C         Calculate aperture photometry on last pass.
	  if(.not.loop) call aper ( pseud2d, nstot, nfast, nslow )
C             
 	  totaltime = (tgetpar+tsearch+tshape+timprove)
	  write(3,1060) totaltime
 	  write(4,1060) totaltime
	  write(luout,1060) totaltime
1060	  format('Total CPU time consumed:',F10.2,' seconds.')
          write(10,1070)infile,tgetpar,tsearch,tshape,timprove,
     *                  totaltime
1070      format(a20,'   T(getp/f)',f5.1,'  T(search)',f5.1,
     *               '  T(shape)',f5.1,'  T(improve)',f5.1,
     *               '  Total',f6.1)
	  call title (line,skyval,.false.,fitr,ngr,ncon,strint,ztot,nums)
	  rewind(2)          
 	  rewind(3)                              
  	  rewind(4)
C
	  call output ( line )
C
C         Now reduce the threshold and loop back
C
	  thresh = thresh/2.**tfac
  	end do                   
C                              
C--------- END OF BIG LOOP ---------------------------------------
C                      
C	If after-burner required, residuals from analytic PSF are computed
C	and stored in RES.
C	
20	if ( burn ) then
C 	
C	If using a fixed (X,Y) coordinate list, read it.
	 if (fixed) then
C	 Read the image frame
 	  call getfits(1,infile,header,nhead,nfast,nslow,numf,nc,line)
C
C	 Initialize arrays, open files etc.
	  call setup ( numf,nc,screen,line,skyval,fitr,ngr,ncon,
     +nfast, nslow )
C
C	 Read the XY list                                 
	  write(luout,'(''Reading XY list ...'')')
	  call xylist(numf, nc, ios )
	  if(ios.ne.0) then
	   fixed = .false.
	   write(luout,'(''SXY file absent or incorrect...'')')
	   goto 15
	  endif
C
	  call htype(line,skyval,.false.,fitr,ngr,ncon)
C
C	 Remove good stars
	  write(luout,'(''Cleaning frame of stars: '',i8)') nstot
	  call clean ( pseud2d, nstot, nfast, nslow, -1)
C             
C	Calculate aperture photometry
C	  call aper ( pseud2d, nstot, nfast, nslow )
	 else            
	   rewind(3)
	   rewind(4)         
	 endif 
C             
C-----------------------
C	Flag all stars close together in groups.  Keep making the distance
C	criterion FITR smaller until the maximum number in a group is less
C	than NFMAX         
C                      
	 fitr = amax1(arect(1),arect(2))
	 fitr = fitr + 2.0      
	 nmax = 10000 
	 write(*,'(''Regrouping ...'')')
C              
	 do while ( nmax.gt.nfmax )
  	  fitr = fitr - 1.0       
	  write(luout,'(''Min distance ='',f8.1)') fitr
	  call regroup( fitr, ngr, nmax )     
	 enddo
C
	 xlim = irect(1)/2 
	 ylim = irect(2)/2
C
C	Calculate normalized PSF residual from PSEUD2D
	 call getres (pseud0d,pseud2d,strint,rmn,rmx,nfast,nslow,irect,
     +arect,ztot,nums)
	 if(nums.eq.0) then
	  write(luout,'(''No suitable PSF stars!'')')
	  goto 30
	 endif
C
	 write(luout,'(/''AFTERBURNER tuned ON!'')')
C
C	Fit multiple stars in a group with enhanced PSF using box size IRECT.
	 call mulfit( pseud2d,pseudmd,ngr,ncon,nfast,nslow,irect )        
C
C	Re-calculate aperture photometry
	 call aperm ( pseudmd, nstot, nfast, nslow )
C
	 call skyadj ( nstot )
C
 	 call title (line,skyval,.true.,fitr,ngr,ncon,strint,ztot,nums)
	 call output ( line )
	endif 
C---------------------                                                
C
C-----  This section skipped if PSF residual not written out ------
C                     
30	if( isub ) then
C
C	Write final Cleaned array.
 	 infile = 'x'//numf(1:nc)//'.fits'
	 call putfits(2,infile,header,nhead,nfast,nslow)
	 close(2)                 
C             
C	If afterburner used, then residual array also written out.
C	Find suitable scale for writing residual PSF to FITS "R" file.
C
 	 if ( wrtres ) then
	  scale=20000.0/(rmx-rmn)
	  zero=-scale*rmn   
	  do j=-nres,nres
  	   jj=nres+j+1   
	   do i=-nres,nres
  	    ii=nres+i+1                                  
	    big(ii,jj)=scale*res(i,j)+zero
	   enddo
	  enddo
	  nx=2*nres+1  
C
	  infile = 'r'//numf(1:nc)//'.fits'
  	  zer=-zero/scale
 	  scl=1.0/scale            
C                                           
C	Create a FITS header for the normalized PSF residual image    
	  call sethead(rhead,numf,nx,nx,zer,scl)
 	  scale=1.0             
	  zero=0.0    
C	Write the normalized PSF residual image
	  call putfits(2,infile,rhead,1,nx,nx)
	  close(2)
	 endif
C                        
	end if
C                     
C                     
900	close(1)
	close(3)                                 
 	close(4)
	if ( .not.screen ) close(luout)
	if(comd) then
          if(instr(5).eq.1)call system('rm shd.'//numf(1:nc))
          if(instr(6).eq.1)call system('rm out.'//numf(1:nc))
          n=1
          do while(infile(n:n).ne.' ')
            n=n+1
          end do
          if(instr(7).eq.1)call system('rm '//infile(1:n-1))
        end if
	fixed = fixedxy
	goto 1
C
995     print 996
996     format(/'*** Fatal error ***'/
     *          'You asked for batch processing but'/
     *          'I cant open the "dophot.bat" file.'/
     *          'Please make one (using batchdophot)'/
     *          'and restart DoPHOT'/)
        go to 999

C
997     print 998
998     format(/'*** Fatal error ***'/
     *          'You asked for "windowed" processing'/
     *          'but I cant open the "windows" file.'/
     *          'Please make one and restart DoPHOT'/)

999	call exit(0)
	end

* (gdb) r
* Starting program: /home3/craig/gnu/f77-e/gcc/f771 -quiet < ../../play/19990826-4.f -O
* [...]
* Breakpoint 2, fancy_abort (
*     file=0x8285220 "../../g77-e/gcc/config/i386/i386.c", line=4399,
*     function=0x82860df "output_fp_cc0_set") at ../../g77-e/gcc/rtl.c:1010
* (gdb) up
* #1  0x8222fab in output_fp_cc0_set (insn=0x8382324)
*     at ../../g77-e/gcc/config/i386/i386.c:4399
* (gdb) p insn
* $1 = 0x3a
* (gdb) up
* #2  0x8222b81 in output_float_compare (insn=0x8382324, operands=0x82acc60)
*     at ../../g77-e/gcc/config/i386/i386.c:4205
* (gdb) p insn
* $2 = 0x8382324
* (gdb) whatis insn
* type = rtx
* (gdb) pr
* (insn 2181 2180 2191 (parallel[
*             (set (cc0)
*                 (compare (reg:SF 8 %st(0))
*                     (mem:SF (plus:SI (reg:SI 6 %ebp)
*                             (const_int -9948 [0xffffd924])) 0)))
*             (clobber (reg:HI 0 %ax))
*         ] ) 29 {*cmpsf_cc_1} (insn_list 2173 (insn_list 2173 (nil)))
*     (expr_list:REG_DEAD (reg:DF 8 %st(0))
*         (expr_list:REG_UNUSED (reg:HI 0 %ax)
*             (nil))))
* (gdb)
