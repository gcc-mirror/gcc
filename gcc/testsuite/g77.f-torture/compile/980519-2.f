* Date: Fri, 17 Apr 1998 14:12:51 +0200
* From: Jean-Paul Jeannot <jeannot@gx-tech.fr>
* Organization: GX Technology France
* To: egcs-bugs@cygnus.com
* Subject: identified bug in g77 on Alpha
* 
* Dear Sir,
* 
* You will find below the assembly code of a simple Fortran routine which
* crashes with segmentation fault when storing the first element 
*       in( jT_f-hd_T     ) = Xsp
* whereas everything is fine when commenting this line.
* 
* The assembly code (generated with 
* -ffast-math -fexpensive-optimizations -fomit-frame-pointer -fno-inline
* or with -O5)
* uses a zapnot instruction to copy an address.
* BUT the zapnot parameter is 15 (copuing 4 bytes) instead of 255 (to copy
* 8 bytes). 
* 
* I guess this is typically a 64 bit issue. As, from my understanding,
* zapnots are used a lot to copy registers, this may create problems
* elsewhere.
* 
* Thanks for your help
* 
* Jean-Paul Jeannot
* 
      subroutine simul_trace( in, Xsp, Ysp, Xrcv, Yrcv )

      common /Idim/ jT_f, jT_l, nT, nT_dim
      common /Idim/ jZ_f, jZ_l, nZ, nZ_dim
      common /Idim/ jZ2_f, jZ2_l, nZ2, nZ2_dim
      common /Idim/ jzs_f, jzs_l, nzs, nzs_dim, l_amp
      common /Idim/ hd_S, hd_Z, hd_T
      common /Idim/ nlay, nlayz
      common /Idim/ n_work
      common /Idim/ nb_calls
	
      real     Xsp, Ysp, Xrcv, Yrcv
      real     in( jT_f-hd_T : jT_l )
	
      in( jT_f-hd_T     ) = Xsp
      in( jT_f-hd_T + 1 ) = Ysp 
      in( jT_f-hd_T + 2 ) = Xrcv
      in( jT_f-hd_T + 3 ) = Yrcv
      end
