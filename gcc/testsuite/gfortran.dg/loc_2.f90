! { dg-do run }
! { dg-options "-std=legacy" }
!
! Series of routines for testing a loc() implementation
program test
  common /errors/errors(12)
  integer i
  logical errors
  errors = .false.
  call testloc
  do i=1,12
     if (errors(i)) then
        call abort()
     endif
  end do
end program test

! Test loc
subroutine testloc
  common /errors/errors(12)
  logical errors
  integer, parameter :: n = 9
  integer, parameter :: m = 10
  integer, parameter :: o = 11
  integer :: offset
  integer :: i,j,k,intsize,realsize,dblsize,chsize,ch8size
  integer itarg1 (n)
  integer itarg2 (m,n)
  integer itarg3 (o,m,n)
  real rtarg1(n)
  real rtarg2(m,n)
  real rtarg3(o,m,n)
  character chtarg1(n)
  character chtarg2(m,n)
  character chtarg3(o,m,n)
  character*8 ch8targ1(n)
  character*8 ch8targ2(m,n)
  character*8 ch8targ3(o,m,n)

  intsize = kind(itarg1(1))
  realsize = kind(rtarg1(1))
  chsize = kind(chtarg1(1))*len(chtarg1(1))
  ch8size = kind(ch8targ1(1))*len(ch8targ1(1))

  do, i=1,n
     offset = i-1
     if (loc(itarg1).ne.loc(itarg1(i))-offset*intsize) then
        ! Error #1
        errors(1) = .true.
     end if
     if (loc(rtarg1).ne.loc(rtarg1(i))-offset*realsize) then
        ! Error #2
        errors(2) = .true.
     end if
     if (loc(chtarg1).ne.loc(chtarg1(i))-offset*chsize) then
        ! Error #3
        errors(3) = .true.
     end if
     if (loc(ch8targ1).ne.loc(ch8targ1(i))-offset*ch8size) then
        ! Error #4
        errors(4) = .true.
     end if

     do, j=1,m
        offset = (j-1)+m*(i-1)
        if (loc(itarg2).ne. &
             loc(itarg2(j,i))-offset*intsize) then
           ! Error #5
           errors(5) = .true.
        end if
        if (loc(rtarg2).ne. &
             loc(rtarg2(j,i))-offset*realsize) then
           ! Error #6
           errors(6) = .true.
        end if
        if (loc(chtarg2).ne. &
             loc(chtarg2(j,i))-offset*chsize) then
           ! Error #7
           errors(7) = .true.
        end if
        if (loc(ch8targ2).ne. &
             loc(ch8targ2(j,i))-offset*ch8size) then
           ! Error #8
           errors(8) = .true.
        end if

        do k=1,o
           offset = (k-1)+o*(j-1)+o*m*(i-1)
           if (loc(itarg3).ne. &
                loc(itarg3(k,j,i))-offset*intsize) then
              ! Error #9
              errors(9) = .true.
           end if
           if (loc(rtarg3).ne. &
                loc(rtarg3(k,j,i))-offset*realsize) then
              ! Error #10
              errors(10) = .true.
           end if
           if (loc(chtarg3).ne. &
                loc(chtarg3(k,j,i))-offset*chsize) then
              ! Error #11
              errors(11) = .true.
           end if
           if (loc(ch8targ3).ne. &
                loc(ch8targ3(k,j,i))-offset*ch8size) then
              ! Error #12
              errors(12) = .true.
           end if

        end do
     end do
  end do

end subroutine testloc

