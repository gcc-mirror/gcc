! Using two spaces between dg-do and run is a hack to keep gfortran-dg-runtest
! from cycling through optimization options for this expensive test.
! { dg-do  run }
! { dg-options "-O3 -fcray-pointer -fbounds-check -fno-inline" }
! { dg-timeout-factor 4 }
!
! Series of routines for testing a Cray pointer implementation
!
! Note: Some of the test cases violate Fortran's alias rules;
! the "-fno-inline option" for now prevents failures.
!
program craytest
  common /errors/errors(400)
  common /foo/foo ! To prevent optimizations
  integer foo
  integer i
  logical errors
  errors = .false.
  foo = 0
  call ptr1
  call ptr2
  call ptr3
  call ptr4
  call ptr5
  call ptr6
  call ptr7
  call ptr8
  call ptr9(9,10,11)
  call ptr10(9,10,11)
  call ptr11(9,10,11)
  call ptr12(9,10,11)
  call ptr13(9,10)
  call parmtest
! NOTE: Tests 1 through 12 were removed from this file
! and placed in loc_1.f90, so we start at 13
  do i=13,400
     if (errors(i)) then
!        print *,"Test",i,"failed."
        STOP 1
     endif
  end do
  if (foo.eq.0) then
!     print *,"Test did not run correctly."
     STOP 2
  endif
end program craytest

! ptr1 through ptr13 that Cray pointees are correctly used with
! a variety of declaration styles
subroutine ptr1
  common /errors/errors(400)
  logical :: errors, intne, realne, chne, ch8ne
  integer :: i,j,k
  integer, parameter :: n = 9
  integer, parameter :: m = 10
  integer, parameter :: o = 11
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
  type drvd
     real r1
     integer i1
     integer i2(5)
  end type drvd
  type(drvd) dtarg1(n)
  type(drvd) dtarg2(m,n)
  type(drvd) dtarg3(o,m,n)

  type(drvd) dpte1(n)
  type(drvd) dpte2(m,n)
  type(drvd) dpte3(o,m,n)
  integer ipte1 (n)
  integer ipte2 (m,n)
  integer ipte3 (o,m,n)
  real rpte1(n)
  real rpte2(m,n)
  real rpte3(o,m,n)
  character chpte1(n)
  character chpte2(m,n)
  character chpte3(o,m,n)
  character*8 ch8pte1(n)
  character*8 ch8pte2(m,n)
  character*8 ch8pte3(o,m,n)

  pointer(iptr1,dpte1)
  pointer(iptr2,dpte2)
  pointer(iptr3,dpte3)
  pointer(iptr4,ipte1)
  pointer(iptr5,ipte2)
  pointer(iptr6,ipte3)
  pointer(iptr7,rpte1)
  pointer(iptr8,rpte2)
  pointer(iptr9,rpte3)
  pointer(iptr10,chpte1)
  pointer(iptr11,chpte2)
  pointer(iptr12,chpte3)
  pointer(iptr13,ch8pte1)
  pointer(iptr14,ch8pte2)
  pointer(iptr15,ch8pte3)

  iptr1 = loc(dtarg1)
  iptr2 = loc(dtarg2)
  iptr3 = loc(dtarg3)
  iptr4 = loc(itarg1)
  iptr5 = loc(itarg2)
  iptr6 = loc(itarg3)
  iptr7 = loc(rtarg1)
  iptr8 = loc(rtarg2)
  iptr9 = loc(rtarg3)
  iptr10= loc(chtarg1)
  iptr11= loc(chtarg2)
  iptr12= loc(chtarg3)
  iptr13= loc(ch8targ1)
  iptr14= loc(ch8targ2)
  iptr15= loc(ch8targ3)


  do, i=1,n
     dpte1(i)%i1=i
     if (intne(dpte1(i)%i1, dtarg1(i)%i1)) then
        ! Error #13
        errors(13) = .true.
     endif

     dtarg1(i)%i1=2*dpte1(i)%i1
     if (intne(dpte1(i)%i1, dtarg1(i)%i1)) then
        ! Error #14
        errors(14) = .true.
     endif

     ipte1(i) = i
     if (intne(ipte1(i), itarg1(i))) then
        ! Error #15
        errors(15) = .true.
     endif

     itarg1(i) = -ipte1(i)
     if (intne(ipte1(i), itarg1(i))) then
        ! Error #16
        errors(16) = .true.
     endif

     rpte1(i) = i * 5.0
     if (realne(rpte1(i), rtarg1(i))) then
        ! Error #17
        errors(17) = .true.
     endif

     rtarg1(i) = i * (-5.0)
     if (realne(rpte1(i), rtarg1(i))) then
        ! Error #18
        errors(18) = .true.
     endif

     chpte1(i) = 'a'
     if (chne(chpte1(i), chtarg1(i))) then
        ! Error #19
        errors(19) = .true.
     endif

     chtarg1(i) = 'z'
     if (chne(chpte1(i), chtarg1(i))) then
        ! Error #20
        errors(20) = .true.
     endif

     ch8pte1(i) = 'aaaaaaaa'
     if (ch8ne(ch8pte1(i), ch8targ1(i))) then
        ! Error #21
        errors(21) = .true.
     endif

     ch8targ1(i) = 'zzzzzzzz'
     if (ch8ne(ch8pte1(i), ch8targ1(i))) then
        ! Error #22
        errors(22) = .true.
     endif

     do, j=1,m
        dpte2(j,i)%r1=1.0
        if (realne(dpte2(j,i)%r1, dtarg2(j,i)%r1)) then
           ! Error #23
           errors(23) = .true.
        endif

        dtarg2(j,i)%r1=2*dpte2(j,i)%r1
        if (realne(dpte2(j,i)%r1, dtarg2(j,i)%r1)) then
           ! Error #24
           errors(24) = .true.
        endif

        ipte2(j,i) = i
        if (intne(ipte2(j,i), itarg2(j,i))) then
           ! Error #25
           errors(25) = .true.
        endif

        itarg2(j,i) = -ipte2(j,i)
        if (intne(ipte2(j,i), itarg2(j,i))) then
           ! Error #26
           errors(26) = .true.
        endif

        rpte2(j,i) = i * (-2.0)
        if (realne(rpte2(j,i), rtarg2(j,i))) then
           ! Error #27
           errors(27) = .true.
        endif

        rtarg2(j,i) = i * (-3.0)
        if (realne(rpte2(j,i), rtarg2(j,i))) then
           ! Error #28
           errors(28) = .true.
        endif

        chpte2(j,i) = 'a'
        if (chne(chpte2(j,i), chtarg2(j,i))) then
           ! Error #29
           errors(29) = .true.
        endif

        chtarg2(j,i) = 'z'
        if (chne(chpte2(j,i), chtarg2(j,i))) then
           ! Error #30
           errors(30) = .true.
        endif

        ch8pte2(j,i) = 'aaaaaaaa'
        if (ch8ne(ch8pte2(j,i), ch8targ2(j,i))) then
           ! Error #31
           errors(31) = .true.
        endif

        ch8targ2(j,i) = 'zzzzzzzz'
        if (ch8ne(ch8pte2(j,i), ch8targ2(j,i))) then
           ! Error #32
           errors(32) = .true.
        endif
        do k=1,o
           dpte3(k,j,i)%i2(1+mod(i,5))=i
           if (intne(dpte3(k,j,i)%i2(1+mod(i,5)), &
                dtarg3(k,j,i)%i2(1+mod(i,5)))) then
              ! Error #33
              errors(33) = .true.
           endif

           dtarg3(k,j,i)%i2(1+mod(i,5))=2*dpte3(k,j,i)%i2(1+mod(i,5))
           if (intne(dpte3(k,j,i)%i2(1+mod(i,5)), &
                dtarg3(k,j,i)%i2(1+mod(i,5)))) then
              ! Error #34
              errors(34) = .true.
           endif

           ipte3(k,j,i) = i
           if (intne(ipte3(k,j,i), itarg3(k,j,i))) then
              ! Error #35
              errors(35) = .true.
           endif

           itarg3(k,j,i) = -ipte3(k,j,i)
           if (intne(ipte3(k,j,i), itarg3(k,j,i))) then
              ! Error #36
              errors(36) = .true.
           endif

           rpte3(k,j,i) = i * 2.0
           if (realne(rpte3(k,j,i), rtarg3(k,j,i))) then
              ! Error #37
              errors(37) = .true.
           endif

           rtarg3(k,j,i) = i * 3.0
           if (realne(rpte3(k,j,i), rtarg3(k,j,i))) then
              ! Error #38
              errors(38) = .true.
           endif

           chpte3(k,j,i) = 'a'
           if (chne(chpte3(k,j,i), chtarg3(k,j,i))) then
              ! Error #39
              errors(39) = .true.
           endif

           chtarg3(k,j,i) = 'z'
           if (chne(chpte3(k,j,i), chtarg3(k,j,i))) then
              ! Error #40
              errors(40) = .true.
           endif

           ch8pte3(k,j,i) = 'aaaaaaaa'
           if (ch8ne(ch8pte3(k,j,i), ch8targ3(k,j,i))) then
              ! Error #41
              errors(41) = .true.
           endif

           ch8targ3(k,j,i) = 'zzzzzzzz'
           if (ch8ne(ch8pte3(k,j,i), ch8targ3(k,j,i))) then
              ! Error #42
              errors(42) = .true.
           endif
        end do
     end do
  end do

  rtarg3 = .5
  ! Vector syntax
  do, i=1,n
     ipte3 = i
     rpte3 = rpte3+1
     do, j=1,m
        do k=1,o
           if (intne(itarg3(k,j,i), i)) then
              ! Error #43
              errors(43) = .true.
           endif

           if (realne(rtarg3(k,j,i), i+.5)) then
              ! Error #44
              errors(44) = .true.
           endif
        end do
     end do
  end do

end subroutine ptr1


subroutine ptr2
  common /errors/errors(400)
  logical :: errors, intne, realne, chne, ch8ne
  integer :: i,j,k
  integer, parameter :: n = 9
  integer, parameter :: m = 10
  integer, parameter :: o = 11
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
  type drvd
     real r1
     integer i1
     integer i2(5)
  end type drvd
  type(drvd) dtarg1(n)
  type(drvd) dtarg2(m,n)
  type(drvd) dtarg3(o,m,n)

  type(drvd) dpte1
  type(drvd) dpte2
  type(drvd) dpte3
  integer ipte1
  integer ipte2
  integer ipte3
  real rpte1
  real rpte2
  real rpte3
  character chpte1
  character chpte2
  character chpte3
  character*8 ch8pte1
  character*8 ch8pte2
  character*8 ch8pte3

  pointer(iptr1,dpte1(n))
  pointer(iptr2,dpte2(m,n))
  pointer(iptr3,dpte3(o,m,n))
  pointer(iptr4,ipte1(n))
  pointer(iptr5,ipte2 (m,n))
  pointer(iptr6,ipte3(o,m,n))
  pointer(iptr7,rpte1(n))
  pointer(iptr8,rpte2(m,n))
  pointer(iptr9,rpte3(o,m,n))
  pointer(iptr10,chpte1(n))
  pointer(iptr11,chpte2(m,n))
  pointer(iptr12,chpte3(o,m,n))
  pointer(iptr13,ch8pte1(n))
  pointer(iptr14,ch8pte2(m,n))
  pointer(iptr15,ch8pte3(o,m,n))

  iptr1 = loc(dtarg1)
  iptr2 = loc(dtarg2)
  iptr3 = loc(dtarg3)
  iptr4 = loc(itarg1)
  iptr5 = loc(itarg2)
  iptr6 = loc(itarg3)
  iptr7 = loc(rtarg1)
  iptr8 = loc(rtarg2)
  iptr9 = loc(rtarg3)
  iptr10= loc(chtarg1)
  iptr11= loc(chtarg2)
  iptr12= loc(chtarg3)
  iptr13= loc(ch8targ1)
  iptr14= loc(ch8targ2)
  iptr15= loc(ch8targ3)

  do, i=1,n
     dpte1(i)%i1=i
     if (intne(dpte1(i)%i1, dtarg1(i)%i1)) then
        ! Error #45
        errors(45) = .true.
     endif

     dtarg1(i)%i1=2*dpte1(i)%i1
     if (intne(dpte1(i)%i1, dtarg1(i)%i1)) then
        ! Error #46
        errors(46) = .true.
     endif

     ipte1(i) = i
     if (intne(ipte1(i), itarg1(i))) then
        ! Error #47
        errors(47) = .true.
     endif

     itarg1(i) = -ipte1(i)
     if (intne(ipte1(i), itarg1(i))) then
        ! Error #48
        errors(48) = .true.
     endif

     rpte1(i) = i * 5.0
     if (realne(rpte1(i), rtarg1(i))) then
        ! Error #49
        errors(49) = .true.
     endif

     rtarg1(i) = i * (-5.0)
     if (realne(rpte1(i), rtarg1(i))) then
        ! Error #50
        errors(50) = .true.
     endif

     chpte1(i) = 'a'
     if (chne(chpte1(i), chtarg1(i))) then
        ! Error #51
        errors(51) = .true.
     endif

     chtarg1(i) = 'z'
     if (chne(chpte1(i), chtarg1(i))) then
        ! Error #52
        errors(52) = .true.
     endif

     ch8pte1(i) = 'aaaaaaaa'
     if (ch8ne(ch8pte1(i), ch8targ1(i))) then
        ! Error #53
        errors(53) = .true.
     endif

     ch8targ1(i) = 'zzzzzzzz'
     if (ch8ne(ch8pte1(i), ch8targ1(i))) then
        ! Error #54
        errors(54) = .true.
     endif

     do, j=1,m
        dpte2(j,i)%r1=1.0
        if (realne(dpte2(j,i)%r1, dtarg2(j,i)%r1)) then
           ! Error #55
           errors(55) = .true.
        endif

        dtarg2(j,i)%r1=2*dpte2(j,i)%r1
        if (realne(dpte2(j,i)%r1, dtarg2(j,i)%r1)) then
           ! Error #56
           errors(56) = .true.
        endif

        ipte2(j,i) = i
        if (intne(ipte2(j,i), itarg2(j,i))) then
           ! Error #57
           errors(57) = .true.
        endif

        itarg2(j,i) = -ipte2(j,i)
        if (intne(ipte2(j,i), itarg2(j,i))) then
           ! Error #58
           errors(58) = .true.
        endif

        rpte2(j,i) = i * (-2.0)
        if (realne(rpte2(j,i), rtarg2(j,i))) then
           ! Error #59
           errors(59) = .true.
        endif

        rtarg2(j,i) = i * (-3.0)
        if (realne(rpte2(j,i), rtarg2(j,i))) then
           ! Error #60
           errors(60) = .true.
        endif

        chpte2(j,i) = 'a'
        if (chne(chpte2(j,i), chtarg2(j,i))) then
           ! Error #61
           errors(61) = .true.
        endif

        chtarg2(j,i) = 'z'
        if (chne(chpte2(j,i), chtarg2(j,i))) then
           ! Error #62
           errors(62) = .true.
        endif

        ch8pte2(j,i) = 'aaaaaaaa'
        if (ch8ne(ch8pte2(j,i), ch8targ2(j,i))) then
           ! Error #63
           errors(63) = .true.
        endif

        ch8targ2(j,i) = 'zzzzzzzz'
        if (ch8ne(ch8pte2(j,i), ch8targ2(j,i))) then
           ! Error #64
           errors(64) = .true.
        endif
        do k=1,o
           dpte3(k,j,i)%i2(1+mod(i,5))=i
           if (intne(dpte3(k,j,i)%i2(1+mod(i,5)), dtarg3(k,j,i)%i2(1+mod(i,5)))) then
              ! Error #65
              errors(65) = .true.
           endif

           dtarg3(k,j,i)%i2(1+mod(i,5))=2*dpte3(k,j,i)%i2(1+mod(i,5))
           if (intne(dpte3(k,j,i)%i2(1+mod(i,5)), dtarg3(k,j,i)%i2(1+mod(i,5)))) then
              ! Error #66
              errors(66) = .true.
           endif

           ipte3(k,j,i) = i
           if (intne(ipte3(k,j,i), itarg3(k,j,i))) then
              ! Error #67
              errors(67) = .true.
           endif

           itarg3(k,j,i) = -ipte3(k,j,i)
           if (intne(ipte3(k,j,i), itarg3(k,j,i))) then
              ! Error #68
              errors(68) = .true.
           endif

           rpte3(k,j,i) = i * 2.0
           if (realne(rpte3(k,j,i), rtarg3(k,j,i))) then
              ! Error #69
              errors(69) = .true.
           endif

           rtarg3(k,j,i) = i * 3.0
           if (realne(rpte3(k,j,i), rtarg3(k,j,i))) then
              ! Error #70
              errors(70) = .true.
           endif

           chpte3(k,j,i) = 'a'
           if (chne(chpte3(k,j,i), chtarg3(k,j,i))) then
              ! Error #71
              errors(71) = .true.
           endif

           chtarg3(k,j,i) = 'z'
           if (chne(chpte3(k,j,i), chtarg3(k,j,i))) then
              ! Error #72
              errors(72) = .true.
           endif

           ch8pte3(k,j,i) = 'aaaaaaaa'
           if (ch8ne(ch8pte3(k,j,i), ch8targ3(k,j,i))) then
              ! Error #73
              errors(73) = .true.
           endif

           ch8targ3(k,j,i) = 'zzzzzzzz'
           if (ch8ne(ch8pte3(k,j,i), ch8targ3(k,j,i))) then
              ! Error #74
              errors(74) = .true.
           endif
        end do
     end do
  end do

  rtarg3 = .5
  ! Vector syntax
  do, i=1,n
     ipte3 = i
     rpte3 = rpte3+1
     do, j=1,m
        do k=1,o
           if (intne(itarg3(k,j,i), i)) then
              ! Error #75
              errors(75) = .true.
           endif

           if (realne(rtarg3(k,j,i), i+.5)) then
              ! Error #76
              errors(76) = .true.
           endif
        end do
     end do
  end do
end subroutine ptr2

subroutine ptr3
  common /errors/errors(400)
  logical :: errors, intne, realne, chne, ch8ne
  integer :: i,j,k
  integer, parameter :: n = 9
  integer, parameter :: m = 10
  integer, parameter :: o = 11
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
  type drvd
     real r1
     integer i1
     integer i2(5)
  end type drvd
  type(drvd) dtarg1(n)
  type(drvd) dtarg2(m,n)
  type(drvd) dtarg3(o,m,n)

  pointer(iptr1,dpte1(n))
  pointer(iptr2,dpte2(m,n))
  pointer(iptr3,dpte3(o,m,n))
  pointer(iptr4,ipte1(n))
  pointer(iptr5,ipte2 (m,n))
  pointer(iptr6,ipte3(o,m,n))
  pointer(iptr7,rpte1(n))
  pointer(iptr8,rpte2(m,n))
  pointer(iptr9,rpte3(o,m,n))
  pointer(iptr10,chpte1(n))
  pointer(iptr11,chpte2(m,n))
  pointer(iptr12,chpte3(o,m,n))
  pointer(iptr13,ch8pte1(n))
  pointer(iptr14,ch8pte2(m,n))
  pointer(iptr15,ch8pte3(o,m,n))

  type(drvd) dpte1
  type(drvd) dpte2
  type(drvd) dpte3
  integer ipte1
  integer ipte2
  integer ipte3
  real rpte1
  real rpte2
  real rpte3
  character chpte1
  character chpte2
  character chpte3
  character*8 ch8pte1
  character*8 ch8pte2
  character*8 ch8pte3

  iptr1 = loc(dtarg1)
  iptr2 = loc(dtarg2)
  iptr3 = loc(dtarg3)
  iptr4 = loc(itarg1)
  iptr5 = loc(itarg2)
  iptr6 = loc(itarg3)
  iptr7 = loc(rtarg1)
  iptr8 = loc(rtarg2)
  iptr9 = loc(rtarg3)
  iptr10= loc(chtarg1)
  iptr11= loc(chtarg2)
  iptr12= loc(chtarg3)
  iptr13= loc(ch8targ1)
  iptr14= loc(ch8targ2)
  iptr15= loc(ch8targ3)

  do, i=1,n
     dpte1(i)%i1=i
     if (intne(dpte1(i)%i1, dtarg1(i)%i1)) then
        ! Error #77
        errors(77) = .true.
     endif

     dtarg1(i)%i1=2*dpte1(i)%i1
     if (intne(dpte1(i)%i1, dtarg1(i)%i1)) then
        ! Error #78
        errors(78) = .true.
     endif

     ipte1(i) = i
     if (intne(ipte1(i), itarg1(i))) then
        ! Error #79
        errors(79) = .true.
     endif

     itarg1(i) = -ipte1(i)
     if (intne(ipte1(i), itarg1(i))) then
        ! Error #80
        errors(80) = .true.
     endif

     rpte1(i) = i * 5.0
     if (realne(rpte1(i), rtarg1(i))) then
        ! Error #81
        errors(81) = .true.
     endif

     rtarg1(i) = i * (-5.0)
     if (realne(rpte1(i), rtarg1(i))) then
        ! Error #82
        errors(82) = .true.
     endif

     chpte1(i) = 'a'
     if (chne(chpte1(i), chtarg1(i))) then
        ! Error #83
        errors(83) = .true.
     endif

     chtarg1(i) = 'z'
     if (chne(chpte1(i), chtarg1(i))) then
        ! Error #84
        errors(84) = .true.
     endif

     ch8pte1(i) = 'aaaaaaaa'
     if (ch8ne(ch8pte1(i), ch8targ1(i))) then
        ! Error #85
        errors(85) = .true.
     endif

     ch8targ1(i) = 'zzzzzzzz'
     if (ch8ne(ch8pte1(i), ch8targ1(i))) then
        ! Error #86
        errors(86) = .true.
     endif

     do, j=1,m
        dpte2(j,i)%r1=1.0
        if (realne(dpte2(j,i)%r1, dtarg2(j,i)%r1)) then
           ! Error #87
           errors(87) = .true.
        endif

        dtarg2(j,i)%r1=2*dpte2(j,i)%r1
        if (realne(dpte2(j,i)%r1, dtarg2(j,i)%r1)) then
           ! Error #88
           errors(88) = .true.
        endif

        ipte2(j,i) = i
        if (intne(ipte2(j,i), itarg2(j,i))) then
           ! Error #89
           errors(89) = .true.
        endif

        itarg2(j,i) = -ipte2(j,i)
        if (intne(ipte2(j,i), itarg2(j,i))) then
           ! Error #90
           errors(90) = .true.
        endif

        rpte2(j,i) = i * (-2.0)
        if (realne(rpte2(j,i), rtarg2(j,i))) then
           ! Error #91
           errors(91) = .true.
        endif

        rtarg2(j,i) = i * (-3.0)
        if (realne(rpte2(j,i), rtarg2(j,i))) then
           ! Error #92
           errors(92) = .true.
        endif

        chpte2(j,i) = 'a'
        if (chne(chpte2(j,i), chtarg2(j,i))) then
           ! Error #93
           errors(93) = .true.
        endif

        chtarg2(j,i) = 'z'
        if (chne(chpte2(j,i), chtarg2(j,i))) then
           ! Error #94
           errors(94) = .true.
        endif

        ch8pte2(j,i) = 'aaaaaaaa'
        if (ch8ne(ch8pte2(j,i), ch8targ2(j,i))) then
           ! Error #95
           errors(95) = .true.
        endif

        ch8targ2(j,i) = 'zzzzzzzz'
        if (ch8ne(ch8pte2(j,i), ch8targ2(j,i))) then
           ! Error #96
           errors(96) = .true.
        endif
        do k=1,o
           dpte3(k,j,i)%i2(1+mod(i,5))=i
           if (intne(dpte3(k,j,i)%i2(1+mod(i,5)), &
                dtarg3(k,j,i)%i2(1+mod(i,5)))) then
              ! Error #97
              errors(97) = .true.
           endif

           dtarg3(k,j,i)%i2(1+mod(i,5))=2*dpte3(k,j,i)%i2(1+mod(i,5))
           if (intne(dpte3(k,j,i)%i2(1+mod(i,5)), &
                dtarg3(k,j,i)%i2(1+mod(i,5)))) then
              ! Error #98
              errors(98) = .true.
           endif

           ipte3(k,j,i) = i
           if (intne(ipte3(k,j,i), itarg3(k,j,i))) then
              ! Error #99
              errors(99) = .true.
           endif

           itarg3(k,j,i) = -ipte3(k,j,i)
           if (intne(ipte3(k,j,i), itarg3(k,j,i))) then
              ! Error #100
              errors(100) = .true.
           endif

           rpte3(k,j,i) = i * 2.0
           if (realne(rpte3(k,j,i), rtarg3(k,j,i))) then
              ! Error #101
              errors(101) = .true.
           endif

           rtarg3(k,j,i) = i * 3.0
           if (realne(rpte3(k,j,i), rtarg3(k,j,i))) then
              ! Error #102
              errors(102) = .true.
           endif

           chpte3(k,j,i) = 'a'
           if (chne(chpte3(k,j,i), chtarg3(k,j,i))) then
              ! Error #103
              errors(103) = .true.
           endif

           chtarg3(k,j,i) = 'z'
           if (chne(chpte3(k,j,i), chtarg3(k,j,i))) then
              ! Error #104
              errors(104) = .true.
           endif

           ch8pte3(k,j,i) = 'aaaaaaaa'
           if (ch8ne(ch8pte3(k,j,i), ch8targ3(k,j,i))) then
              ! Error #105
              errors(105) = .true.
           endif

           ch8targ3(k,j,i) = 'zzzzzzzz'
           if (ch8ne(ch8pte3(k,j,i), ch8targ3(k,j,i))) then
              ! Error #106
              errors(106) = .true.
           endif
        end do
     end do
  end do

  rtarg3 = .5
  ! Vector syntax
  do, i=1,n
     ipte3 = i
     rpte3 = rpte3+1
     do, j=1,m
        do k=1,o
           if (intne(itarg3(k,j,i), i)) then
              ! Error #107
              errors(107) = .true.
           endif

           if (realne(rtarg3(k,j,i), i+.5)) then
              ! Error #108
              errors(108) = .true.
           endif
        end do
     end do
  end do
end subroutine ptr3

subroutine ptr4
  common /errors/errors(400)
  logical :: errors, intne, realne, chne, ch8ne
  integer :: i,j,k
  integer, parameter :: n = 9
  integer, parameter :: m = 10
  integer, parameter :: o = 11
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
  type drvd
     real r1
     integer i1
     integer i2(5)
  end type drvd
  type(drvd) dtarg1(n)
  type(drvd) dtarg2(m,n)
  type(drvd) dtarg3(o,m,n)

  pointer(iptr1,dpte1),(iptr2,dpte2),(iptr3,dpte3)
  pointer    (iptr4,ipte1),  (iptr5,ipte2) ,(iptr6,ipte3),(iptr7,rpte1)
  pointer(iptr8,rpte2)
  pointer(iptr9,rpte3),(iptr10,chpte1)
  pointer(iptr11,chpte2),(iptr12,chpte3),(iptr13,ch8pte1)
  pointer(iptr14,ch8pte2)
  pointer(iptr15,ch8pte3)

  type(drvd) dpte1(n)
  type(drvd) dpte2(m,n)
  type(drvd) dpte3(o,m,n)
  integer ipte1 (n)
  integer ipte2 (m,n)
  integer ipte3 (o,m,n)
  real rpte1(n)
  real rpte2(m,n)
  real rpte3(o,m,n)
  character chpte1(n)
  character chpte2(m,n)
  character chpte3(o,m,n)
  character*8 ch8pte1(n)
  character*8 ch8pte2(m,n)
  character*8 ch8pte3(o,m,n)

  iptr1 = loc(dtarg1)
  iptr2 = loc(dtarg2)
  iptr3 = loc(dtarg3)
  iptr4 = loc(itarg1)
  iptr5 = loc(itarg2)
  iptr6 = loc(itarg3)
  iptr7 = loc(rtarg1)
  iptr8 = loc(rtarg2)
  iptr9 = loc(rtarg3)
  iptr10= loc(chtarg1)
  iptr11= loc(chtarg2)
  iptr12= loc(chtarg3)
  iptr13= loc(ch8targ1)
  iptr14= loc(ch8targ2)
  iptr15= loc(ch8targ3)


  do, i=1,n
     dpte1(i)%i1=i
     if (intne(dpte1(i)%i1, dtarg1(i)%i1)) then
        ! Error #109
        errors(109) = .true.
     endif

     dtarg1(i)%i1=2*dpte1(i)%i1
     if (intne(dpte1(i)%i1, dtarg1(i)%i1)) then
        ! Error #110
        errors(110) = .true.
     endif

     ipte1(i) = i
     if (intne(ipte1(i), itarg1(i))) then
        ! Error #111
        errors(111) = .true.
     endif

     itarg1(i) = -ipte1(i)
     if (intne(ipte1(i), itarg1(i))) then
        ! Error #112
        errors(112) = .true.
     endif

     rpte1(i) = i * 5.0
     if (realne(rpte1(i), rtarg1(i))) then
        ! Error #113
        errors(113) = .true.
     endif

     rtarg1(i) = i * (-5.0)
     if (realne(rpte1(i), rtarg1(i))) then
        ! Error #114
        errors(114) = .true.
     endif

     chpte1(i) = 'a'
     if (chne(chpte1(i), chtarg1(i))) then
        ! Error #115
        errors(115) = .true.
     endif

     chtarg1(i) = 'z'
     if (chne(chpte1(i), chtarg1(i))) then
        ! Error #116
        errors(116) = .true.
     endif

     ch8pte1(i) = 'aaaaaaaa'
     if (ch8ne(ch8pte1(i), ch8targ1(i))) then
        ! Error #117
        errors(117) = .true.
     endif

     ch8targ1(i) = 'zzzzzzzz'
     if (ch8ne(ch8pte1(i), ch8targ1(i))) then
        ! Error #118
        errors(118) = .true.
     endif

     do, j=1,m
        dpte2(j,i)%r1=1.0
        if (realne(dpte2(j,i)%r1, dtarg2(j,i)%r1)) then
           ! Error #119
           errors(119) = .true.
        endif

        dtarg2(j,i)%r1=2*dpte2(j,i)%r1
        if (realne(dpte2(j,i)%r1, dtarg2(j,i)%r1)) then
           ! Error #120
           errors(120) = .true.
        endif

        ipte2(j,i) = i
        if (intne(ipte2(j,i), itarg2(j,i))) then
           ! Error #121
           errors(121) = .true.
        endif

        itarg2(j,i) = -ipte2(j,i)
        if (intne(ipte2(j,i), itarg2(j,i))) then
           ! Error #122
           errors(122) = .true.
        endif

        rpte2(j,i) = i * (-2.0)
        if (realne(rpte2(j,i), rtarg2(j,i))) then
           ! Error #123
           errors(123) = .true.
        endif

        rtarg2(j,i) = i * (-3.0)
        if (realne(rpte2(j,i), rtarg2(j,i))) then
           ! Error #124
           errors(124) = .true.
        endif

        chpte2(j,i) = 'a'
        if (chne(chpte2(j,i), chtarg2(j,i))) then
           ! Error #125
           errors(125) = .true.
        endif

        chtarg2(j,i) = 'z'
        if (chne(chpte2(j,i), chtarg2(j,i))) then
           ! Error #126
           errors(126) = .true.
        endif

        ch8pte2(j,i) = 'aaaaaaaa'
        if (ch8ne(ch8pte2(j,i), ch8targ2(j,i))) then
           ! Error #127
           errors(127) = .true.
        endif

        ch8targ2(j,i) = 'zzzzzzzz'
        if (ch8ne(ch8pte2(j,i), ch8targ2(j,i))) then
           ! Error #128
           errors(128) = .true.
        endif
        do k=1,o
           dpte3(k,j,i)%i2(1+mod(i,5))=i
           if (intne(dpte3(k,j,i)%i2(1+mod(i,5)), &
                dtarg3(k,j,i)%i2(1+mod(i,5)))) then
              ! Error #129
              errors(129) = .true.
           endif

           dtarg3(k,j,i)%i2(1+mod(i,5))=2*dpte3(k,j,i)%i2(1+mod(i,5))
           if (intne(dpte3(k,j,i)%i2(1+mod(i,5)), &
                dtarg3(k,j,i)%i2(1+mod(i,5)))) then
              ! Error #130
              errors(130) = .true.
           endif

           ipte3(k,j,i) = i
           if (intne(ipte3(k,j,i), itarg3(k,j,i))) then
              ! Error #131
              errors(131) = .true.
           endif

           itarg3(k,j,i) = -ipte3(k,j,i)
           if (intne(ipte3(k,j,i), itarg3(k,j,i))) then
              ! Error #132
              errors(132) = .true.
           endif

           rpte3(k,j,i) = i * 2.0
           if (realne(rpte3(k,j,i), rtarg3(k,j,i))) then
              ! Error #133
              errors(133) = .true.
           endif

           rtarg3(k,j,i) = i * 3.0
           if (realne(rpte3(k,j,i), rtarg3(k,j,i))) then
              ! Error #134
              errors(134) = .true.
           endif

           chpte3(k,j,i) = 'a'
           if (chne(chpte3(k,j,i), chtarg3(k,j,i))) then
              ! Error #135
              errors(135) = .true.
           endif

           chtarg3(k,j,i) = 'z'
           if (chne(chpte3(k,j,i), chtarg3(k,j,i))) then
              ! Error #136
              errors(136) = .true.
           endif

           ch8pte3(k,j,i) = 'aaaaaaaa'
           if (ch8ne(ch8pte3(k,j,i), ch8targ3(k,j,i))) then
              ! Error #137
              errors(137) = .true.
           endif

           ch8targ3(k,j,i) = 'zzzzzzzz'
           if (ch8ne(ch8pte3(k,j,i), ch8targ3(k,j,i))) then
              ! Error #138
              errors(138) = .true.
           endif
        end do
     end do
  end do

  rtarg3 = .5
  ! Vector syntax
  do, i=1,n
     ipte3 = i
     rpte3 = rpte3+1
     do, j=1,m
        do k=1,o
           if (intne(itarg3(k,j,i), i)) then
              ! Error #139
              errors(139) = .true.
           endif

           if (realne(rtarg3(k,j,i), i+.5)) then
              ! Error #140
              errors(140) = .true.
           endif
        end do
     end do
  end do

end subroutine ptr4

subroutine ptr5
  common /errors/errors(400)
  logical :: errors, intne, realne, chne, ch8ne
  integer :: i,j,k
  integer, parameter :: n = 9
  integer, parameter :: m = 10
  integer, parameter :: o = 11
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
  type drvd
     real r1
     integer i1
     integer i2(5)
  end type drvd
  type(drvd) dtarg1(n)
  type(drvd) dtarg2(m,n)
  type(drvd) dtarg3(o,m,n)

  type(drvd) dpte1(*)
  type(drvd) dpte2(m,*)
  type(drvd) dpte3(o,m,*)
  integer ipte1 (*)
  integer ipte2 (m,*)
  integer ipte3 (o,m,*)
  real rpte1(*)
  real rpte2(m,*)
  real rpte3(o,m,*)
  character chpte1(*)
  character chpte2(m,*)
  character chpte3(o,m,*)
  character*8 ch8pte1(*)
  character*8 ch8pte2(m,*)
  character*8 ch8pte3(o,m,*)

  pointer(iptr1,dpte1)
  pointer(iptr2,dpte2)
  pointer(iptr3,dpte3)
  pointer(iptr4,ipte1)
  pointer(iptr5,ipte2)
  pointer(iptr6,ipte3)
  pointer(iptr7,rpte1)
  pointer(iptr8,rpte2)
  pointer(iptr9,rpte3)
  pointer(iptr10,chpte1)
  pointer(iptr11,chpte2)
  pointer(iptr12,chpte3)
  pointer(iptr13,ch8pte1)
  pointer(iptr14,ch8pte2)
  pointer(iptr15,ch8pte3)

  iptr1 = loc(dtarg1)
  iptr2 = loc(dtarg2)
  iptr3 = loc(dtarg3)
  iptr4 = loc(itarg1)
  iptr5 = loc(itarg2)
  iptr6 = loc(itarg3)
  iptr7 = loc(rtarg1)
  iptr8 = loc(rtarg2)
  iptr9 = loc(rtarg3)
  iptr10= loc(chtarg1)
  iptr11= loc(chtarg2)
  iptr12= loc(chtarg3)
  iptr13= loc(ch8targ1)
  iptr14= loc(ch8targ2)
  iptr15= loc(ch8targ3)


  do, i=1,n
     dpte1(i)%i1=i
     if (intne(dpte1(i)%i1, dtarg1(i)%i1)) then
        ! Error #141
        errors(141) = .true.
     endif

     dtarg1(i)%i1=2*dpte1(i)%i1
     if (intne(dpte1(i)%i1, dtarg1(i)%i1)) then
        ! Error #142
        errors(142) = .true.
     endif

     ipte1(i) = i
     if (intne(ipte1(i), itarg1(i))) then
        ! Error #143
        errors(143) = .true.
     endif

     itarg1(i) = -ipte1(i)
     if (intne(ipte1(i), itarg1(i))) then
        ! Error #144
        errors(144) = .true.
     endif

     rpte1(i) = i * 5.0
     if (realne(rpte1(i), rtarg1(i))) then
        ! Error #145
        errors(145) = .true.
     endif

     rtarg1(i) = i * (-5.0)
     if (realne(rpte1(i), rtarg1(i))) then
        ! Error #146
        errors(146) = .true.
     endif

     chpte1(i) = 'a'
     if (chne(chpte1(i), chtarg1(i))) then
        ! Error #147
        errors(147) = .true.
     endif

     chtarg1(i) = 'z'
     if (chne(chpte1(i), chtarg1(i))) then
        ! Error #148
        errors(148) = .true.
     endif

     ch8pte1(i) = 'aaaaaaaa'
     if (ch8ne(ch8pte1(i), ch8targ1(i))) then
        ! Error #149
        errors(149) = .true.
     endif

     ch8targ1(i) = 'zzzzzzzz'
     if (ch8ne(ch8pte1(i), ch8targ1(i))) then
        ! Error #150
        errors(150) = .true.
     endif

     do, j=1,m
        dpte2(j,i)%r1=1.0
        if (realne(dpte2(j,i)%r1, dtarg2(j,i)%r1)) then
           ! Error #151
           errors(151) = .true.
        endif

        dtarg2(j,i)%r1=2*dpte2(j,i)%r1
        if (realne(dpte2(j,i)%r1, dtarg2(j,i)%r1)) then
           ! Error #152
           errors(152) = .true.
        endif

        ipte2(j,i) = i
        if (intne(ipte2(j,i), itarg2(j,i))) then
           ! Error #153
           errors(153) = .true.
        endif

        itarg2(j,i) = -ipte2(j,i)
        if (intne(ipte2(j,i), itarg2(j,i))) then
           ! Error #154
           errors(154) = .true.
        endif

        rpte2(j,i) = i * (-2.0)
        if (realne(rpte2(j,i), rtarg2(j,i))) then
           ! Error #155
           errors(155) = .true.
        endif

        rtarg2(j,i) = i * (-3.0)
        if (realne(rpte2(j,i), rtarg2(j,i))) then
           ! Error #156
           errors(156) = .true.
        endif

        chpte2(j,i) = 'a'
        if (chne(chpte2(j,i), chtarg2(j,i))) then
           ! Error #157
           errors(157) = .true.
        endif

        chtarg2(j,i) = 'z'
        if (chne(chpte2(j,i), chtarg2(j,i))) then
           ! Error #158
           errors(158) = .true.
        endif

        ch8pte2(j,i) = 'aaaaaaaa'
        if (ch8ne(ch8pte2(j,i), ch8targ2(j,i))) then
           ! Error #159
           errors(159) = .true.
        endif

        ch8targ2(j,i) = 'zzzzzzzz'
        if (ch8ne(ch8pte2(j,i), ch8targ2(j,i))) then
           ! Error #160
           errors(160) = .true.
        endif
        do k=1,o
           dpte3(k,j,i)%i2(1+mod(i,5))=i
           if (intne(dpte3(k,j,i)%i2(1+mod(i,5)), &
                dtarg3(k,j,i)%i2(1+mod(i,5)))) then
              ! Error #161
              errors(161) = .true.
           endif

           dtarg3(k,j,i)%i2(1+mod(i,5))=2*dpte3(k,j,i)%i2(1+mod(i,5))
           if (intne(dpte3(k,j,i)%i2(1+mod(i,5)), &
                dtarg3(k,j,i)%i2(1+mod(i,5)))) then
              ! Error #162
              errors(162) = .true.
           endif

           ipte3(k,j,i) = i
           if (intne(ipte3(k,j,i), itarg3(k,j,i))) then
              ! Error #163
              errors(163) = .true.
           endif

           itarg3(k,j,i) = -ipte3(k,j,i)
           if (intne(ipte3(k,j,i), itarg3(k,j,i))) then
              ! Error #164
              errors(164) = .true.
           endif

           rpte3(k,j,i) = i * 2.0
           if (realne(rpte3(k,j,i), rtarg3(k,j,i))) then
              ! Error #165
              errors(165) = .true.
           endif

           rtarg3(k,j,i) = i * 3.0
           if (realne(rpte3(k,j,i), rtarg3(k,j,i))) then
              ! Error #166
              errors(166) = .true.
           endif

           chpte3(k,j,i) = 'a'
           if (chne(chpte3(k,j,i), chtarg3(k,j,i))) then
              ! Error #167
              errors(167) = .true.
           endif

           chtarg3(k,j,i) = 'z'
           if (chne(chpte3(k,j,i), chtarg3(k,j,i))) then
              ! Error #168
              errors(168) = .true.
           endif

           ch8pte3(k,j,i) = 'aaaaaaaa'
           if (ch8ne(ch8pte3(k,j,i), ch8targ3(k,j,i))) then
              ! Error #169
              errors(169) = .true.
           endif

           ch8targ3(k,j,i) = 'zzzzzzzz'
           if (ch8ne(ch8pte3(k,j,i), ch8targ3(k,j,i))) then
              ! Error #170
              errors(170) = .true.
           endif
        end do
     end do
  end do

end subroutine ptr5


subroutine ptr6
  common /errors/errors(400)
  logical :: errors, intne, realne, chne, ch8ne
  integer :: i,j,k
  integer, parameter :: n = 9
  integer, parameter :: m = 10
  integer, parameter :: o = 11
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
  type drvd
     real r1
     integer i1
     integer i2(5)
  end type drvd
  type(drvd) dtarg1(n)
  type(drvd) dtarg2(m,n)
  type(drvd) dtarg3(o,m,n)

  type(drvd) dpte1
  type(drvd) dpte2
  type(drvd) dpte3
  integer ipte1
  integer ipte2
  integer ipte3
  real rpte1
  real rpte2
  real rpte3
  character chpte1
  character chpte2
  character chpte3
  character*8 ch8pte1
  character*8 ch8pte2
  character*8 ch8pte3

  pointer(iptr1,dpte1(*))
  pointer(iptr2,dpte2(m,*))
  pointer(iptr3,dpte3(o,m,*))
  pointer(iptr4,ipte1(*))
  pointer(iptr5,ipte2 (m,*))
  pointer(iptr6,ipte3(o,m,*))
  pointer(iptr7,rpte1(*))
  pointer(iptr8,rpte2(m,*))
  pointer(iptr9,rpte3(o,m,*))
  pointer(iptr10,chpte1(*))
  pointer(iptr11,chpte2(m,*))
  pointer(iptr12,chpte3(o,m,*))
  pointer(iptr13,ch8pte1(*))
  pointer(iptr14,ch8pte2(m,*))
  pointer(iptr15,ch8pte3(o,m,*))

  iptr1 = loc(dtarg1)
  iptr2 = loc(dtarg2)
  iptr3 = loc(dtarg3)
  iptr4 = loc(itarg1)
  iptr5 = loc(itarg2)
  iptr6 = loc(itarg3)
  iptr7 = loc(rtarg1)
  iptr8 = loc(rtarg2)
  iptr9 = loc(rtarg3)
  iptr10= loc(chtarg1)
  iptr11= loc(chtarg2)
  iptr12= loc(chtarg3)
  iptr13= loc(ch8targ1)
  iptr14= loc(ch8targ2)
  iptr15= loc(ch8targ3)

  do, i=1,n
     dpte1(i)%i1=i
     if (intne(dpte1(i)%i1, dtarg1(i)%i1)) then
        ! Error #171
        errors(171) = .true.
     endif

     dtarg1(i)%i1=2*dpte1(i)%i1
     if (intne(dpte1(i)%i1, dtarg1(i)%i1)) then
        ! Error #172
        errors(172) = .true.
     endif

     ipte1(i) = i
     if (intne(ipte1(i), itarg1(i))) then
        ! Error #173
        errors(173) = .true.
     endif

     itarg1(i) = -ipte1(i)
     if (intne(ipte1(i), itarg1(i))) then
        ! Error #174
        errors(174) = .true.
     endif

     rpte1(i) = i * 5.0
     if (realne(rpte1(i), rtarg1(i))) then
        ! Error #175
        errors(175) = .true.
     endif

     rtarg1(i) = i * (-5.0)
     if (realne(rpte1(i), rtarg1(i))) then
        ! Error #176
        errors(176) = .true.
     endif

     chpte1(i) = 'a'
     if (chne(chpte1(i), chtarg1(i))) then
        ! Error #177
        errors(177) = .true.
     endif

     chtarg1(i) = 'z'
     if (chne(chpte1(i), chtarg1(i))) then
        ! Error #178
        errors(178) = .true.
     endif

     ch8pte1(i) = 'aaaaaaaa'
     if (ch8ne(ch8pte1(i), ch8targ1(i))) then
        ! Error #179
        errors(179) = .true.
     endif

     ch8targ1(i) = 'zzzzzzzz'
     if (ch8ne(ch8pte1(i), ch8targ1(i))) then
        ! Error #180
        errors(180) = .true.
     endif

     do, j=1,m
        dpte2(j,i)%r1=1.0
        if (realne(dpte2(j,i)%r1, dtarg2(j,i)%r1)) then
           ! Error #181
           errors(181) = .true.
        endif

        dtarg2(j,i)%r1=2*dpte2(j,i)%r1
        if (realne(dpte2(j,i)%r1, dtarg2(j,i)%r1)) then
           ! Error #182
           errors(182) = .true.
        endif

        ipte2(j,i) = i
        if (intne(ipte2(j,i), itarg2(j,i))) then
           ! Error #183
           errors(183) = .true.
        endif

        itarg2(j,i) = -ipte2(j,i)
        if (intne(ipte2(j,i), itarg2(j,i))) then
           ! Error #184
           errors(184) = .true.
        endif

        rpte2(j,i) = i * (-2.0)
        if (realne(rpte2(j,i), rtarg2(j,i))) then
           ! Error #185
           errors(185) = .true.
        endif

        rtarg2(j,i) = i * (-3.0)
        if (realne(rpte2(j,i), rtarg2(j,i))) then
           ! Error #186
           errors(186) = .true.
        endif

        chpte2(j,i) = 'a'
        if (chne(chpte2(j,i), chtarg2(j,i))) then
           ! Error #187
           errors(187) = .true.
        endif

        chtarg2(j,i) = 'z'
        if (chne(chpte2(j,i), chtarg2(j,i))) then
           ! Error #188
           errors(188) = .true.
        endif

        ch8pte2(j,i) = 'aaaaaaaa'
        if (ch8ne(ch8pte2(j,i), ch8targ2(j,i))) then
           ! Error #189
           errors(189) = .true.
        endif

        ch8targ2(j,i) = 'zzzzzzzz'
        if (ch8ne(ch8pte2(j,i), ch8targ2(j,i))) then
           ! Error #190
           errors(190) = .true.
        endif
        do k=1,o
           dpte3(k,j,i)%i2(1+mod(i,5))=i
           if (intne(dpte3(k,j,i)%i2(1+mod(i,5)), &
                dtarg3(k,j,i)%i2(1+mod(i,5)))) then
              ! Error #191
              errors(191) = .true.
           endif

           dtarg3(k,j,i)%i2(1+mod(i,5))=2*dpte3(k,j,i)%i2(1+mod(i,5))
           if (intne(dpte3(k,j,i)%i2(1+mod(i,5)), &
                dtarg3(k,j,i)%i2(1+mod(i,5)))) then
              ! Error #192
              errors(192) = .true.
           endif

           ipte3(k,j,i) = i
           if (intne(ipte3(k,j,i), itarg3(k,j,i))) then
              ! Error #193
              errors(193) = .true.
           endif

           itarg3(k,j,i) = -ipte3(k,j,i)
           if (intne(ipte3(k,j,i), itarg3(k,j,i))) then
              ! Error #194
              errors(194) = .true.
           endif

           rpte3(k,j,i) = i * 2.0
           if (realne(rpte3(k,j,i), rtarg3(k,j,i))) then
              ! Error #195
              errors(195) = .true.
           endif

           rtarg3(k,j,i) = i * 3.0
           if (realne(rpte3(k,j,i), rtarg3(k,j,i))) then
              ! Error #196
              errors(196) = .true.
           endif

           chpte3(k,j,i) = 'a'
           if (chne(chpte3(k,j,i), chtarg3(k,j,i))) then
              ! Error #197
              errors(197) = .true.
           endif

           chtarg3(k,j,i) = 'z'
           if (chne(chpte3(k,j,i), chtarg3(k,j,i))) then
              ! Error #198
              errors(198) = .true.
           endif

           ch8pte3(k,j,i) = 'aaaaaaaa'
           if (ch8ne(ch8pte3(k,j,i), ch8targ3(k,j,i))) then
              ! Error #199
              errors(199) = .true.
           endif

           ch8targ3(k,j,i) = 'zzzzzzzz'
           if (ch8ne(ch8pte3(k,j,i), ch8targ3(k,j,i))) then
              ! Error #200
              errors(200) = .true.
           endif
        end do
     end do
  end do

end subroutine ptr6

subroutine ptr7
  common /errors/errors(400)
  logical :: errors, intne, realne, chne, ch8ne
  integer :: i,j,k
  integer, parameter :: n = 9
  integer, parameter :: m = 10
  integer, parameter :: o = 11
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
  type drvd
     real r1
     integer i1
     integer i2(5)
  end type drvd
  type(drvd) dtarg1(n)
  type(drvd) dtarg2(m,n)
  type(drvd) dtarg3(o,m,n)

  pointer(iptr1,dpte1(*))
  pointer(iptr2,dpte2(m,*))
  pointer(iptr3,dpte3(o,m,*))
  pointer(iptr4,ipte1(*))
  pointer(iptr5,ipte2 (m,*))
  pointer(iptr6,ipte3(o,m,*))
  pointer(iptr7,rpte1(*))
  pointer(iptr8,rpte2(m,*))
  pointer(iptr9,rpte3(o,m,*))
  pointer(iptr10,chpte1(*))
  pointer(iptr11,chpte2(m,*))
  pointer(iptr12,chpte3(o,m,*))
  pointer(iptr13,ch8pte1(*))
  pointer(iptr14,ch8pte2(m,*))
  pointer(iptr15,ch8pte3(o,m,*))

  type(drvd) dpte1
  type(drvd) dpte2
  type(drvd) dpte3
  integer ipte1
  integer ipte2
  integer ipte3
  real rpte1
  real rpte2
  real rpte3
  character chpte1
  character chpte2
  character chpte3
  character*8 ch8pte1
  character*8 ch8pte2
  character*8 ch8pte3

  iptr1 = loc(dtarg1)
  iptr2 = loc(dtarg2)
  iptr3 = loc(dtarg3)
  iptr4 = loc(itarg1)
  iptr5 = loc(itarg2)
  iptr6 = loc(itarg3)
  iptr7 = loc(rtarg1)
  iptr8 = loc(rtarg2)
  iptr9 = loc(rtarg3)
  iptr10= loc(chtarg1)
  iptr11= loc(chtarg2)
  iptr12= loc(chtarg3)
  iptr13= loc(ch8targ1)
  iptr14= loc(ch8targ2)
  iptr15= loc(ch8targ3)

  do, i=1,n
     dpte1(i)%i1=i
     if (intne(dpte1(i)%i1, dtarg1(i)%i1)) then
        ! Error #201
        errors(201) = .true.
     endif

     dtarg1(i)%i1=2*dpte1(i)%i1
     if (intne(dpte1(i)%i1, dtarg1(i)%i1)) then
        ! Error #202
        errors(202) = .true.
     endif

     ipte1(i) = i
     if (intne(ipte1(i), itarg1(i))) then
        ! Error #203
        errors(203) = .true.
     endif

     itarg1(i) = -ipte1(i)
     if (intne(ipte1(i), itarg1(i))) then
        ! Error #204
        errors(204) = .true.
     endif

     rpte1(i) = i * 5.0
     if (realne(rpte1(i), rtarg1(i))) then
        ! Error #205
        errors(205) = .true.
     endif

     rtarg1(i) = i * (-5.0)
     if (realne(rpte1(i), rtarg1(i))) then
        ! Error #206
        errors(206) = .true.
     endif

     chpte1(i) = 'a'
     if (chne(chpte1(i), chtarg1(i))) then
        ! Error #207
        errors(207) = .true.
     endif

     chtarg1(i) = 'z'
     if (chne(chpte1(i), chtarg1(i))) then
        ! Error #208
        errors(208) = .true.
     endif

     ch8pte1(i) = 'aaaaaaaa'
     if (ch8ne(ch8pte1(i), ch8targ1(i))) then
        ! Error #209
        errors(209) = .true.
     endif

     ch8targ1(i) = 'zzzzzzzz'
     if (ch8ne(ch8pte1(i), ch8targ1(i))) then
        ! Error #210
        errors(210) = .true.
     endif

     do, j=1,m
        dpte2(j,i)%r1=1.0
        if (realne(dpte2(j,i)%r1, dtarg2(j,i)%r1)) then
           ! Error #211
           errors(211) = .true.
        endif

        dtarg2(j,i)%r1=2*dpte2(j,i)%r1
        if (realne(dpte2(j,i)%r1, dtarg2(j,i)%r1)) then
           ! Error #212
           errors(212) = .true.
        endif

        ipte2(j,i) = i
        if (intne(ipte2(j,i), itarg2(j,i))) then
           ! Error #213
           errors(213) = .true.
        endif

        itarg2(j,i) = -ipte2(j,i)
        if (intne(ipte2(j,i), itarg2(j,i))) then
           ! Error #214
           errors(214) = .true.
        endif

        rpte2(j,i) = i * (-2.0)
        if (realne(rpte2(j,i), rtarg2(j,i))) then
           ! Error #215
           errors(215) = .true.
        endif

        rtarg2(j,i) = i * (-3.0)
        if (realne(rpte2(j,i), rtarg2(j,i))) then
           ! Error #216
           errors(216) = .true.
        endif

        chpte2(j,i) = 'a'
        if (chne(chpte2(j,i), chtarg2(j,i))) then
           ! Error #217
           errors(217) = .true.
        endif

        chtarg2(j,i) = 'z'
        if (chne(chpte2(j,i), chtarg2(j,i))) then
           ! Error #218
           errors(218) = .true.
        endif

        ch8pte2(j,i) = 'aaaaaaaa'
        if (ch8ne(ch8pte2(j,i), ch8targ2(j,i))) then
           ! Error #219
           errors(219) = .true.
        endif

        ch8targ2(j,i) = 'zzzzzzzz'
        if (ch8ne(ch8pte2(j,i), ch8targ2(j,i))) then
           ! Error #220
           errors(220) = .true.
        endif
        do k=1,o
           dpte3(k,j,i)%i2(1+mod(i,5))=i
           if (intne(dpte3(k,j,i)%i2(1+mod(i,5)), &
                dtarg3(k,j,i)%i2(1+mod(i,5)))) then
              ! Error #221
              errors(221) = .true.
           endif

           dtarg3(k,j,i)%i2(1+mod(i,5))=2*dpte3(k,j,i)%i2(1+mod(i,5))
           if (intne(dpte3(k,j,i)%i2(1+mod(i,5)), &
                dtarg3(k,j,i)%i2(1+mod(i,5)))) then
              ! Error #222
              errors(222) = .true.
           endif

           ipte3(k,j,i) = i
           if (intne(ipte3(k,j,i), itarg3(k,j,i))) then
              ! Error #223
              errors(223) = .true.
           endif

           itarg3(k,j,i) = -ipte3(k,j,i)
           if (intne(ipte3(k,j,i), itarg3(k,j,i))) then
              ! Error #224
              errors(224) = .true.
           endif

           rpte3(k,j,i) = i * 2.0
           if (realne(rpte3(k,j,i), rtarg3(k,j,i))) then
              ! Error #225
              errors(225) = .true.
           endif

           rtarg3(k,j,i) = i * 3.0
           if (realne(rpte3(k,j,i), rtarg3(k,j,i))) then
              ! Error #226
              errors(226) = .true.
           endif

           chpte3(k,j,i) = 'a'
           if (chne(chpte3(k,j,i), chtarg3(k,j,i))) then
              ! Error #227
              errors(227) = .true.
           endif

           chtarg3(k,j,i) = 'z'
           if (chne(chpte3(k,j,i), chtarg3(k,j,i))) then
              ! Error #228
              errors(228) = .true.
           endif

           ch8pte3(k,j,i) = 'aaaaaaaa'
           if (ch8ne(ch8pte3(k,j,i), ch8targ3(k,j,i))) then
              ! Error #229
              errors(229) = .true.
           endif

           ch8targ3(k,j,i) = 'zzzzzzzz'
           if (ch8ne(ch8pte3(k,j,i), ch8targ3(k,j,i))) then
              ! Error #230
              errors(230) = .true.
           endif
        end do
     end do
  end do

end subroutine ptr7

subroutine ptr8
  common /errors/errors(400)
  logical :: errors, intne, realne, chne, ch8ne
  integer :: i,j,k
  integer, parameter :: n = 9
  integer, parameter :: m = 10
  integer, parameter :: o = 11
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
  type drvd
     real r1
     integer i1
     integer i2(5)
  end type drvd
  type(drvd) dtarg1(n)
  type(drvd) dtarg2(m,n)
  type(drvd) dtarg3(o,m,n)

  pointer(iptr1,dpte1)
  pointer(iptr2,dpte2)
  pointer(iptr3,dpte3)
  pointer(iptr4,ipte1)
  pointer(iptr5,ipte2)
  pointer(iptr6,ipte3)
  pointer(iptr7,rpte1)
  pointer(iptr8,rpte2)
  pointer(iptr9,rpte3)
  pointer(iptr10,chpte1)
  pointer(iptr11,chpte2)
  pointer(iptr12,chpte3)
  pointer(iptr13,ch8pte1)
  pointer(iptr14,ch8pte2)
  pointer(iptr15,ch8pte3)

  type(drvd) dpte1(*)
  type(drvd) dpte2(m,*)
  type(drvd) dpte3(o,m,*)
  integer ipte1 (*)
  integer ipte2 (m,*)
  integer ipte3 (o,m,*)
  real rpte1(*)
  real rpte2(m,*)
  real rpte3(o,m,*)
  character chpte1(*)
  character chpte2(m,*)
  character chpte3(o,m,*)
  character*8 ch8pte1(*)
  character*8 ch8pte2(m,*)
  character*8 ch8pte3(o,m,*)

  iptr1 = loc(dtarg1)
  iptr2 = loc(dtarg2)
  iptr3 = loc(dtarg3)
  iptr4 = loc(itarg1)
  iptr5 = loc(itarg2)
  iptr6 = loc(itarg3)
  iptr7 = loc(rtarg1)
  iptr8 = loc(rtarg2)
  iptr9 = loc(rtarg3)
  iptr10= loc(chtarg1)
  iptr11= loc(chtarg2)
  iptr12= loc(chtarg3)
  iptr13= loc(ch8targ1)
  iptr14= loc(ch8targ2)
  iptr15= loc(ch8targ3)


  do, i=1,n
     dpte1(i)%i1=i
     if (intne(dpte1(i)%i1, dtarg1(i)%i1)) then
        ! Error #231
        errors(231) = .true.
     endif

     dtarg1(i)%i1=2*dpte1(i)%i1
     if (intne(dpte1(i)%i1, dtarg1(i)%i1)) then
        ! Error #232
        errors(232) = .true.
     endif

     ipte1(i) = i
     if (intne(ipte1(i), itarg1(i))) then
        ! Error #233
        errors(233) = .true.
     endif

     itarg1(i) = -ipte1(i)
     if (intne(ipte1(i), itarg1(i))) then
        ! Error #234
        errors(234) = .true.
     endif

     rpte1(i) = i * 5.0
     if (realne(rpte1(i), rtarg1(i))) then
        ! Error #235
        errors(235) = .true.
     endif

     rtarg1(i) = i * (-5.0)
     if (realne(rpte1(i), rtarg1(i))) then
        ! Error #236
        errors(236) = .true.
     endif

     chpte1(i) = 'a'
     if (chne(chpte1(i), chtarg1(i))) then
        ! Error #237
        errors(237) = .true.
     endif

     chtarg1(i) = 'z'
     if (chne(chpte1(i), chtarg1(i))) then
        ! Error #238
        errors(238) = .true.
     endif

     ch8pte1(i) = 'aaaaaaaa'
     if (ch8ne(ch8pte1(i), ch8targ1(i))) then
        ! Error #239
        errors(239) = .true.
     endif

     ch8targ1(i) = 'zzzzzzzz'
     if (ch8ne(ch8pte1(i), ch8targ1(i))) then
        ! Error #240
        errors(240) = .true.
     endif

     do, j=1,m
        dpte2(j,i)%r1=1.0
        if (realne(dpte2(j,i)%r1, dtarg2(j,i)%r1)) then
           ! Error #241
           errors(241) = .true.
        endif

        dtarg2(j,i)%r1=2*dpte2(j,i)%r1
        if (realne(dpte2(j,i)%r1, dtarg2(j,i)%r1)) then
           ! Error #242
           errors(242) = .true.
        endif

        ipte2(j,i) = i
        if (intne(ipte2(j,i), itarg2(j,i))) then
           ! Error #243
           errors(243) = .true.
        endif

        itarg2(j,i) = -ipte2(j,i)
        if (intne(ipte2(j,i), itarg2(j,i))) then
           ! Error #244
           errors(244) = .true.
        endif

        rpte2(j,i) = i * (-2.0)
        if (realne(rpte2(j,i), rtarg2(j,i))) then
           ! Error #245
           errors(245) = .true.
        endif

        rtarg2(j,i) = i * (-3.0)
        if (realne(rpte2(j,i), rtarg2(j,i))) then
           ! Error #246
           errors(246) = .true.
        endif

        chpte2(j,i) = 'a'
        if (chne(chpte2(j,i), chtarg2(j,i))) then
           ! Error #247
           errors(247) = .true.
        endif

        chtarg2(j,i) = 'z'
        if (chne(chpte2(j,i), chtarg2(j,i))) then
           ! Error #248
           errors(248) = .true.
        endif

        ch8pte2(j,i) = 'aaaaaaaa'
        if (ch8ne(ch8pte2(j,i), ch8targ2(j,i))) then
           ! Error #249
           errors(249) = .true.
        endif

        ch8targ2(j,i) = 'zzzzzzzz'
        if (ch8ne(ch8pte2(j,i), ch8targ2(j,i))) then
           ! Error #250
           errors(250) = .true.
        endif
        do k=1,o
           dpte3(k,j,i)%i2(1+mod(i,5))=i
           if (intne(dpte3(k,j,i)%i2(1+mod(i,5)), &
                dtarg3(k,j,i)%i2(1+mod(i,5)))) then
              ! Error #251
              errors(251) = .true.
           endif

           dtarg3(k,j,i)%i2(1+mod(i,5))=2*dpte3(k,j,i)%i2(1+mod(i,5))
           if (intne(dpte3(k,j,i)%i2(1+mod(i,5)), &
                dtarg3(k,j,i)%i2(1+mod(i,5)))) then
              ! Error #252
              errors(252) = .true.
           endif

           ipte3(k,j,i) = i
           if (intne(ipte3(k,j,i), itarg3(k,j,i))) then
              ! Error #253
              errors(253) = .true.
           endif

           itarg3(k,j,i) = -ipte3(k,j,i)
           if (intne(ipte3(k,j,i), itarg3(k,j,i))) then
              ! Error #254
              errors(254) = .true.
           endif

           rpte3(k,j,i) = i * 2.0
           if (realne(rpte3(k,j,i), rtarg3(k,j,i))) then
              ! Error #255
              errors(255) = .true.
           endif

           rtarg3(k,j,i) = i * 3.0
           if (realne(rpte3(k,j,i), rtarg3(k,j,i))) then
              ! Error #256
              errors(256) = .true.
           endif

           chpte3(k,j,i) = 'a'
           if (chne(chpte3(k,j,i), chtarg3(k,j,i))) then
              ! Error #257
              errors(257) = .true.
           endif

           chtarg3(k,j,i) = 'z'
           if (chne(chpte3(k,j,i), chtarg3(k,j,i))) then
              ! Error #258
              errors(258) = .true.
           endif

           ch8pte3(k,j,i) = 'aaaaaaaa'
           if (ch8ne(ch8pte3(k,j,i), ch8targ3(k,j,i))) then
              ! Error #259
              errors(259) = .true.
           endif

           ch8targ3(k,j,i) = 'zzzzzzzz'
           if (ch8ne(ch8pte3(k,j,i), ch8targ3(k,j,i))) then
              ! Error #260
              errors(260) = .true.
           endif
        end do
     end do
  end do
end subroutine ptr8


subroutine ptr9(nnn,mmm,ooo)
  common /errors/errors(400)
  logical :: errors, intne, realne, chne, ch8ne
  integer :: i,j,k
  integer :: nnn,mmm,ooo
  integer, parameter :: n = 9
  integer, parameter :: m = 10
  integer, parameter :: o = 11
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
  type drvd
     real r1
     integer i1
     integer i2(5)
  end type drvd
  type(drvd) dtarg1(n)
  type(drvd) dtarg2(m,n)
  type(drvd) dtarg3(o,m,n)

  type(drvd) dpte1(nnn)
  type(drvd) dpte2(mmm,nnn)
  type(drvd) dpte3(ooo,mmm,nnn)
  integer ipte1 (nnn)
  integer ipte2 (mmm,nnn)
  integer ipte3 (ooo,mmm,nnn)
  real rpte1(nnn)
  real rpte2(mmm,nnn)
  real rpte3(ooo,mmm,nnn)
  character chpte1(nnn)
  character chpte2(mmm,nnn)
  character chpte3(ooo,mmm,nnn)
  character*8 ch8pte1(nnn)
  character*8 ch8pte2(mmm,nnn)
  character*8 ch8pte3(ooo,mmm,nnn)

  pointer(iptr1,dpte1)
  pointer(iptr2,dpte2)
  pointer(iptr3,dpte3)
  pointer(iptr4,ipte1)
  pointer(iptr5,ipte2)
  pointer(iptr6,ipte3)
  pointer(iptr7,rpte1)
  pointer(iptr8,rpte2)
  pointer(iptr9,rpte3)
  pointer(iptr10,chpte1)
  pointer(iptr11,chpte2)
  pointer(iptr12,chpte3)
  pointer(iptr13,ch8pte1)
  pointer(iptr14,ch8pte2)
  pointer(iptr15,ch8pte3)

  iptr1 = loc(dtarg1)
  iptr2 = loc(dtarg2)
  iptr3 = loc(dtarg3)
  iptr4 = loc(itarg1)
  iptr5 = loc(itarg2)
  iptr6 = loc(itarg3)
  iptr7 = loc(rtarg1)
  iptr8 = loc(rtarg2)
  iptr9 = loc(rtarg3)
  iptr10= loc(chtarg1)
  iptr11= loc(chtarg2)
  iptr12= loc(chtarg3)
  iptr13= loc(ch8targ1)
  iptr14= loc(ch8targ2)
  iptr15= loc(ch8targ3)


  do, i=1,n
     dpte1(i)%i1=i
     if (intne(dpte1(i)%i1, dtarg1(i)%i1)) then
        ! Error #261
        errors(261) = .true.
     endif

     dtarg1(i)%i1=2*dpte1(i)%i1
     if (intne(dpte1(i)%i1, dtarg1(i)%i1)) then
        ! Error #262
        errors(262) = .true.
     endif

     ipte1(i) = i
     if (intne(ipte1(i), itarg1(i))) then
        ! Error #263
        errors(263) = .true.
     endif

     itarg1(i) = -ipte1(i)
     if (intne(ipte1(i), itarg1(i))) then
        ! Error #264
        errors(264) = .true.
     endif

     rpte1(i) = i * 5.0
     if (realne(rpte1(i), rtarg1(i))) then
        ! Error #265
        errors(265) = .true.
     endif

     rtarg1(i) = i * (-5.0)
     if (realne(rpte1(i), rtarg1(i))) then
        ! Error #266
        errors(266) = .true.
     endif

     chpte1(i) = 'a'
     if (chne(chpte1(i), chtarg1(i))) then
        ! Error #267
        errors(267) = .true.
     endif

     chtarg1(i) = 'z'
     if (chne(chpte1(i), chtarg1(i))) then
        ! Error #268
        errors(268) = .true.
     endif

     ch8pte1(i) = 'aaaaaaaa'
     if (ch8ne(ch8pte1(i), ch8targ1(i))) then
        ! Error #269
        errors(269) = .true.
     endif

     ch8targ1(i) = 'zzzzzzzz'
     if (ch8ne(ch8pte1(i), ch8targ1(i))) then
        ! Error #270
        errors(270) = .true.
     endif

     do, j=1,m
        dpte2(j,i)%r1=1.0
        if (realne(dpte2(j,i)%r1, dtarg2(j,i)%r1)) then
           ! Error #271
           errors(271) = .true.
        endif

        dtarg2(j,i)%r1=2*dpte2(j,i)%r1
        if (realne(dpte2(j,i)%r1, dtarg2(j,i)%r1)) then
           ! Error #272
           errors(272) = .true.
        endif

        ipte2(j,i) = i
        if (intne(ipte2(j,i), itarg2(j,i))) then
           ! Error #273
           errors(273) = .true.
        endif

        itarg2(j,i) = -ipte2(j,i)
        if (intne(ipte2(j,i), itarg2(j,i))) then
           ! Error #274
           errors(274) = .true.
        endif

        rpte2(j,i) = i * (-2.0)
        if (realne(rpte2(j,i), rtarg2(j,i))) then
           ! Error #275
           errors(275) = .true.
        endif

        rtarg2(j,i) = i * (-3.0)
        if (realne(rpte2(j,i), rtarg2(j,i))) then
           ! Error #276
           errors(276) = .true.
        endif

        chpte2(j,i) = 'a'
        if (chne(chpte2(j,i), chtarg2(j,i))) then
           ! Error #277
           errors(277) = .true.
        endif

        chtarg2(j,i) = 'z'
        if (chne(chpte2(j,i), chtarg2(j,i))) then
           ! Error #278
           errors(278) = .true.
        endif

        ch8pte2(j,i) = 'aaaaaaaa'
        if (ch8ne(ch8pte2(j,i), ch8targ2(j,i))) then
           ! Error #279
           errors(279) = .true.
        endif

        ch8targ2(j,i) = 'zzzzzzzz'
        if (ch8ne(ch8pte2(j,i), ch8targ2(j,i))) then
           ! Error #280
           errors(280) = .true.
        endif
        do k=1,o
           dpte3(k,j,i)%i2(1+mod(i,5))=i
           if (intne(dpte3(k,j,i)%i2(1+mod(i,5)), &
                dtarg3(k,j,i)%i2(1+mod(i,5)))) then
              ! Error #281
              errors(281) = .true.
           endif

           dtarg3(k,j,i)%i2(1+mod(i,5))=2*dpte3(k,j,i)%i2(1+mod(i,5))
           if (intne(dpte3(k,j,i)%i2(1+mod(i,5)), &
                dtarg3(k,j,i)%i2(1+mod(i,5)))) then
              ! Error #282
              errors(282) = .true.
           endif

           ipte3(k,j,i) = i
           if (intne(ipte3(k,j,i), itarg3(k,j,i))) then
              ! Error #283
              errors(283) = .true.
           endif

           itarg3(k,j,i) = -ipte3(k,j,i)
           if (intne(ipte3(k,j,i), itarg3(k,j,i))) then
              ! Error #284
              errors(284) = .true.
           endif

           rpte3(k,j,i) = i * 2.0
           if (realne(rpte3(k,j,i), rtarg3(k,j,i))) then
              ! Error #285
              errors(285) = .true.
           endif

           rtarg3(k,j,i) = i * 3.0
           if (realne(rpte3(k,j,i), rtarg3(k,j,i))) then
              ! Error #286
              errors(286) = .true.
           endif

           chpte3(k,j,i) = 'a'
           if (chne(chpte3(k,j,i), chtarg3(k,j,i))) then
              ! Error #287
              errors(287) = .true.
           endif

           chtarg3(k,j,i) = 'z'
           if (chne(chpte3(k,j,i), chtarg3(k,j,i))) then
              ! Error #288
              errors(288) = .true.
           endif

           ch8pte3(k,j,i) = 'aaaaaaaa'
           if (ch8ne(ch8pte3(k,j,i), ch8targ3(k,j,i))) then
              ! Error #289
              errors(289) = .true.
           endif

           ch8targ3(k,j,i) = 'zzzzzzzz'
           if (ch8ne(ch8pte3(k,j,i), ch8targ3(k,j,i))) then
              ! Error #290
              errors(290) = .true.
           endif
        end do
     end do
  end do

  rtarg3 = .5
  ! Vector syntax
  do, i=1,n
     ipte3 = i
     rpte3 = rpte3+1
     do, j=1,m
        do k=1,o
           if (intne(itarg3(k,j,i), i)) then
              ! Error #291
              errors(291) = .true.
           endif

           if (realne(rtarg3(k,j,i), i+.5)) then
              ! Error #292
              errors(292) = .true.
           endif
        end do
     end do
  end do

end subroutine ptr9

subroutine ptr10(nnn,mmm,ooo)
  common /errors/errors(400)
  logical :: errors, intne, realne, chne, ch8ne
  integer :: i,j,k
  integer :: nnn,mmm,ooo
  integer, parameter :: n = 9
  integer, parameter :: m = 10
  integer, parameter :: o = 11
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
  type drvd
     real r1
     integer i1
     integer i2(5)
  end type drvd
  type(drvd) dtarg1(n)
  type(drvd) dtarg2(m,n)
  type(drvd) dtarg3(o,m,n)

  type(drvd) dpte1
  type(drvd) dpte2
  type(drvd) dpte3
  integer ipte1
  integer ipte2
  integer ipte3
  real rpte1
  real rpte2
  real rpte3
  character chpte1
  character chpte2
  character chpte3
  character*8 ch8pte1
  character*8 ch8pte2
  character*8 ch8pte3

  pointer(iptr1,dpte1(nnn))
  pointer(iptr2,dpte2(mmm,nnn))
  pointer(iptr3,dpte3(ooo,mmm,nnn))
  pointer(iptr4,ipte1(nnn))
  pointer(iptr5,ipte2 (mmm,nnn))
  pointer(iptr6,ipte3(ooo,mmm,nnn))
  pointer(iptr7,rpte1(nnn))
  pointer(iptr8,rpte2(mmm,nnn))
  pointer(iptr9,rpte3(ooo,mmm,nnn))
  pointer(iptr10,chpte1(nnn))
  pointer(iptr11,chpte2(mmm,nnn))
  pointer(iptr12,chpte3(ooo,mmm,nnn))
  pointer(iptr13,ch8pte1(nnn))
  pointer(iptr14,ch8pte2(mmm,nnn))
  pointer(iptr15,ch8pte3(ooo,mmm,nnn))

  iptr1 = loc(dtarg1)
  iptr2 = loc(dtarg2)
  iptr3 = loc(dtarg3)
  iptr4 = loc(itarg1)
  iptr5 = loc(itarg2)
  iptr6 = loc(itarg3)
  iptr7 = loc(rtarg1)
  iptr8 = loc(rtarg2)
  iptr9 = loc(rtarg3)
  iptr10= loc(chtarg1)
  iptr11= loc(chtarg2)
  iptr12= loc(chtarg3)
  iptr13= loc(ch8targ1)
  iptr14= loc(ch8targ2)
  iptr15= loc(ch8targ3)

  do, i=1,n
     dpte1(i)%i1=i
     if (intne(dpte1(i)%i1, dtarg1(i)%i1)) then
        ! Error #293
        errors(293) = .true.
     endif

     dtarg1(i)%i1=2*dpte1(i)%i1
     if (intne(dpte1(i)%i1, dtarg1(i)%i1)) then
        ! Error #294
        errors(294) = .true.
     endif

     ipte1(i) = i
     if (intne(ipte1(i), itarg1(i))) then
        ! Error #295
        errors(295) = .true.
     endif

     itarg1(i) = -ipte1(i)
     if (intne(ipte1(i), itarg1(i))) then
        ! Error #296
        errors(296) = .true.
     endif

     rpte1(i) = i * 5.0
     if (realne(rpte1(i), rtarg1(i))) then
        ! Error #297
        errors(297) = .true.
     endif

     rtarg1(i) = i * (-5.0)
     if (realne(rpte1(i), rtarg1(i))) then
        ! Error #298
        errors(298) = .true.
     endif

     chpte1(i) = 'a'
     if (chne(chpte1(i), chtarg1(i))) then
        ! Error #299
        errors(299) = .true.
     endif

     chtarg1(i) = 'z'
     if (chne(chpte1(i), chtarg1(i))) then
        ! Error #300
        errors(300) = .true.
     endif

     ch8pte1(i) = 'aaaaaaaa'
     if (ch8ne(ch8pte1(i), ch8targ1(i))) then
        ! Error #301
        errors(301) = .true.
     endif

     ch8targ1(i) = 'zzzzzzzz'
     if (ch8ne(ch8pte1(i), ch8targ1(i))) then
        ! Error #302
        errors(302) = .true.
     endif

     do, j=1,m
        dpte2(j,i)%r1=1.0
        if (realne(dpte2(j,i)%r1, dtarg2(j,i)%r1)) then
           ! Error #303
           errors(303) = .true.
        endif

        dtarg2(j,i)%r1=2*dpte2(j,i)%r1
        if (realne(dpte2(j,i)%r1, dtarg2(j,i)%r1)) then
           ! Error #304
           errors(304) = .true.
        endif

        ipte2(j,i) = i
        if (intne(ipte2(j,i), itarg2(j,i))) then
           ! Error #305
           errors(305) = .true.
        endif

        itarg2(j,i) = -ipte2(j,i)
        if (intne(ipte2(j,i), itarg2(j,i))) then
           ! Error #306
           errors(306) = .true.
        endif

        rpte2(j,i) = i * (-2.0)
        if (realne(rpte2(j,i), rtarg2(j,i))) then
           ! Error #307
           errors(307) = .true.
        endif

        rtarg2(j,i) = i * (-3.0)
        if (realne(rpte2(j,i), rtarg2(j,i))) then
           ! Error #308
           errors(308) = .true.
        endif

        chpte2(j,i) = 'a'
        if (chne(chpte2(j,i), chtarg2(j,i))) then
           ! Error #309
           errors(309) = .true.
        endif

        chtarg2(j,i) = 'z'
        if (chne(chpte2(j,i), chtarg2(j,i))) then
           ! Error #310
           errors(310) = .true.
        endif

        ch8pte2(j,i) = 'aaaaaaaa'
        if (ch8ne(ch8pte2(j,i), ch8targ2(j,i))) then
           ! Error #311
           errors(311) = .true.
        endif

        ch8targ2(j,i) = 'zzzzzzzz'
        if (ch8ne(ch8pte2(j,i), ch8targ2(j,i))) then
           ! Error #312
           errors(312) = .true.
        endif
        do k=1,o
           dpte3(k,j,i)%i2(1+mod(i,5))=i
           if (intne(dpte3(k,j,i)%i2(1+mod(i,5)), &
                dtarg3(k,j,i)%i2(1+mod(i,5)))) then
              ! Error #313
              errors(313) = .true.
           endif

           dtarg3(k,j,i)%i2(1+mod(i,5))=2*dpte3(k,j,i)%i2(1+mod(i,5))
           if (intne(dpte3(k,j,i)%i2(1+mod(i,5)), &
                dtarg3(k,j,i)%i2(1+mod(i,5)))) then
              ! Error #314
              errors(314) = .true.
           endif

           ipte3(k,j,i) = i
           if (intne(ipte3(k,j,i), itarg3(k,j,i))) then
              ! Error #315
              errors(315) = .true.
           endif

           itarg3(k,j,i) = -ipte3(k,j,i)
           if (intne(ipte3(k,j,i), itarg3(k,j,i))) then
              ! Error #316
              errors(316) = .true.
           endif

           rpte3(k,j,i) = i * 2.0
           if (realne(rpte3(k,j,i), rtarg3(k,j,i))) then
              ! Error #317
              errors(317) = .true.
           endif

           rtarg3(k,j,i) = i * 3.0
           if (realne(rpte3(k,j,i), rtarg3(k,j,i))) then
              ! Error #318
              errors(318) = .true.
           endif

           chpte3(k,j,i) = 'a'
           if (chne(chpte3(k,j,i), chtarg3(k,j,i))) then
              ! Error #319
              errors(319) = .true.
           endif

           chtarg3(k,j,i) = 'z'
           if (chne(chpte3(k,j,i), chtarg3(k,j,i))) then
              ! Error #320
              errors(320) = .true.
           endif

           ch8pte3(k,j,i) = 'aaaaaaaa'
           if (ch8ne(ch8pte3(k,j,i), ch8targ3(k,j,i))) then
              ! Error #321
              errors(321) = .true.
           endif

           ch8targ3(k,j,i) = 'zzzzzzzz'
           if (ch8ne(ch8pte3(k,j,i), ch8targ3(k,j,i))) then
              ! Error #322
              errors(322) = .true.
           endif
        end do
     end do
  end do

  rtarg3 = .5
  ! Vector syntax
  do, i=1,n
     ipte3 = i
     rpte3 = rpte3+1
     do, j=1,m
        do k=1,o
           if (intne(itarg3(k,j,i), i)) then
              ! Error #323
              errors(323) = .true.
           endif

           if (realne(rtarg3(k,j,i), i+.5)) then
              ! Error #324
              errors(324) = .true.
           endif
        end do
     end do
  end do
end subroutine ptr10

subroutine ptr11(nnn,mmm,ooo)
  common /errors/errors(400)
  logical :: errors, intne, realne, chne, ch8ne
  integer :: i,j,k
  integer :: nnn,mmm,ooo
  integer, parameter :: n = 9
  integer, parameter :: m = 10
  integer, parameter :: o = 11
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
  type drvd
     real r1
     integer i1
     integer i2(5)
  end type drvd
  type(drvd) dtarg1(n)
  type(drvd) dtarg2(m,n)
  type(drvd) dtarg3(o,m,n)

  pointer(iptr1,dpte1(nnn))
  pointer(iptr2,dpte2(mmm,nnn))
  pointer(iptr3,dpte3(ooo,mmm,nnn))
  pointer(iptr4,ipte1(nnn))
  pointer(iptr5,ipte2 (mmm,nnn))
  pointer(iptr6,ipte3(ooo,mmm,nnn))
  pointer(iptr7,rpte1(nnn))
  pointer(iptr8,rpte2(mmm,nnn))
  pointer(iptr9,rpte3(ooo,mmm,nnn))
  pointer(iptr10,chpte1(nnn))
  pointer(iptr11,chpte2(mmm,nnn))
  pointer(iptr12,chpte3(ooo,mmm,nnn))
  pointer(iptr13,ch8pte1(nnn))
  pointer(iptr14,ch8pte2(mmm,nnn))
  pointer(iptr15,ch8pte3(ooo,mmm,nnn))

  type(drvd) dpte1
  type(drvd) dpte2
  type(drvd) dpte3
  integer ipte1
  integer ipte2
  integer ipte3
  real rpte1
  real rpte2
  real rpte3
  character chpte1
  character chpte2
  character chpte3
  character*8 ch8pte1
  character*8 ch8pte2
  character*8 ch8pte3

  iptr1 = loc(dtarg1)
  iptr2 = loc(dtarg2)
  iptr3 = loc(dtarg3)
  iptr4 = loc(itarg1)
  iptr5 = loc(itarg2)
  iptr6 = loc(itarg3)
  iptr7 = loc(rtarg1)
  iptr8 = loc(rtarg2)
  iptr9 = loc(rtarg3)
  iptr10= loc(chtarg1)
  iptr11= loc(chtarg2)
  iptr12= loc(chtarg3)
  iptr13= loc(ch8targ1)
  iptr14= loc(ch8targ2)
  iptr15= loc(ch8targ3)

  do, i=1,n
     dpte1(i)%i1=i
     if (intne(dpte1(i)%i1, dtarg1(i)%i1)) then
        ! Error #325
        errors(325) = .true.
     endif

     dtarg1(i)%i1=2*dpte1(i)%i1
     if (intne(dpte1(i)%i1, dtarg1(i)%i1)) then
        ! Error #326
        errors(326) = .true.
     endif

     ipte1(i) = i
     if (intne(ipte1(i), itarg1(i))) then
        ! Error #327
        errors(327) = .true.
     endif

     itarg1(i) = -ipte1(i)
     if (intne(ipte1(i), itarg1(i))) then
        ! Error #328
        errors(328) = .true.
     endif

     rpte1(i) = i * 5.0
     if (realne(rpte1(i), rtarg1(i))) then
        ! Error #329
        errors(329) = .true.
     endif

     rtarg1(i) = i * (-5.0)
     if (realne(rpte1(i), rtarg1(i))) then
        ! Error #330
        errors(330) = .true.
     endif

     chpte1(i) = 'a'
     if (chne(chpte1(i), chtarg1(i))) then
        ! Error #331
        errors(331) = .true.
     endif

     chtarg1(i) = 'z'
     if (chne(chpte1(i), chtarg1(i))) then
        ! Error #332
        errors(332) = .true.
     endif

     ch8pte1(i) = 'aaaaaaaa'
     if (ch8ne(ch8pte1(i), ch8targ1(i))) then
        ! Error #333
        errors(333) = .true.
     endif

     ch8targ1(i) = 'zzzzzzzz'
     if (ch8ne(ch8pte1(i), ch8targ1(i))) then
        ! Error #334
        errors(334) = .true.
     endif

     do, j=1,m
        dpte2(j,i)%r1=1.0
        if (realne(dpte2(j,i)%r1, dtarg2(j,i)%r1)) then
           ! Error #335
           errors(335) = .true.
        endif

        dtarg2(j,i)%r1=2*dpte2(j,i)%r1
        if (realne(dpte2(j,i)%r1, dtarg2(j,i)%r1)) then
           ! Error #336
           errors(336) = .true.
        endif

        ipte2(j,i) = i
        if (intne(ipte2(j,i), itarg2(j,i))) then
           ! Error #337
           errors(337) = .true.
        endif

        itarg2(j,i) = -ipte2(j,i)
        if (intne(ipte2(j,i), itarg2(j,i))) then
           ! Error #338
           errors(338) = .true.
        endif

        rpte2(j,i) = i * (-2.0)
        if (realne(rpte2(j,i), rtarg2(j,i))) then
           ! Error #339
           errors(339) = .true.
        endif

        rtarg2(j,i) = i * (-3.0)
        if (realne(rpte2(j,i), rtarg2(j,i))) then
           ! Error #340
           errors(340) = .true.
        endif

        chpte2(j,i) = 'a'
        if (chne(chpte2(j,i), chtarg2(j,i))) then
           ! Error #341
           errors(341) = .true.
        endif

        chtarg2(j,i) = 'z'
        if (chne(chpte2(j,i), chtarg2(j,i))) then
           ! Error #342
           errors(342) = .true.
        endif

        ch8pte2(j,i) = 'aaaaaaaa'
        if (ch8ne(ch8pte2(j,i), ch8targ2(j,i))) then
           ! Error #343
           errors(343) = .true.
        endif

        ch8targ2(j,i) = 'zzzzzzzz'
        if (ch8ne(ch8pte2(j,i), ch8targ2(j,i))) then
           ! Error #344
           errors(344) = .true.
        endif
        do k=1,o
           dpte3(k,j,i)%i2(1+mod(i,5))=i
           if (intne(dpte3(k,j,i)%i2(1+mod(i,5)), &
                dtarg3(k,j,i)%i2(1+mod(i,5)))) then
              ! Error #345
              errors(345) = .true.
           endif

           dtarg3(k,j,i)%i2(1+mod(i,5))=2*dpte3(k,j,i)%i2(1+mod(i,5))
           if (intne(dpte3(k,j,i)%i2(1+mod(i,5)), &
                dtarg3(k,j,i)%i2(1+mod(i,5)))) then
              ! Error #346
              errors(346) = .true.
           endif

           ipte3(k,j,i) = i
           if (intne(ipte3(k,j,i), itarg3(k,j,i))) then
              ! Error #347
              errors(347) = .true.
           endif

           itarg3(k,j,i) = -ipte3(k,j,i)
           if (intne(ipte3(k,j,i), itarg3(k,j,i))) then
              ! Error #348
              errors(348) = .true.
           endif

           rpte3(k,j,i) = i * 2.0
           if (realne(rpte3(k,j,i), rtarg3(k,j,i))) then
              ! Error #349
              errors(349) = .true.
           endif

           rtarg3(k,j,i) = i * 3.0
           if (realne(rpte3(k,j,i), rtarg3(k,j,i))) then
              ! Error #350
              errors(350) = .true.
           endif

           chpte3(k,j,i) = 'a'
           if (chne(chpte3(k,j,i), chtarg3(k,j,i))) then
              ! Error #351
              errors(351) = .true.
           endif

           chtarg3(k,j,i) = 'z'
           if (chne(chpte3(k,j,i), chtarg3(k,j,i))) then
              ! Error #352
              errors(352) = .true.
           endif

           ch8pte3(k,j,i) = 'aaaaaaaa'
           if (ch8ne(ch8pte3(k,j,i), ch8targ3(k,j,i))) then
              ! Error #353
              errors(353) = .true.
           endif

           ch8targ3(k,j,i) = 'zzzzzzzz'
           if (ch8ne(ch8pte3(k,j,i), ch8targ3(k,j,i))) then
              ! Error #354
              errors(354) = .true.
           endif
        end do
     end do
  end do

  rtarg3 = .5
  ! Vector syntax
  do, i=1,n
     ipte3 = i
     rpte3 = rpte3+1
     do, j=1,m
        do k=1,o
           if (intne(itarg3(k,j,i), i)) then
              ! Error #355
              errors(355) = .true.
           endif

           if (realne(rtarg3(k,j,i), i+.5)) then
              ! Error #356
              errors(356) = .true.
           endif
        end do
     end do
  end do
end subroutine ptr11

subroutine ptr12(nnn,mmm,ooo)
  common /errors/errors(400)
  logical :: errors, intne, realne, chne, ch8ne
  integer :: i,j,k
  integer :: nnn,mmm,ooo
  integer, parameter :: n = 9
  integer, parameter :: m = 10
  integer, parameter :: o = 11
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
  type drvd
     real r1
     integer i1
     integer i2(5)
  end type drvd
  type(drvd) dtarg1(n)
  type(drvd) dtarg2(m,n)
  type(drvd) dtarg3(o,m,n)

  pointer(iptr1,dpte1)
  pointer(iptr2,dpte2)
  pointer(iptr3,dpte3)
  pointer(iptr4,ipte1)
  pointer(iptr5,ipte2)
  pointer(iptr6,ipte3)
  pointer(iptr7,rpte1)
  pointer(iptr8,rpte2)
  pointer(iptr9,rpte3)
  pointer(iptr10,chpte1)
  pointer(iptr11,chpte2)
  pointer(iptr12,chpte3)
  pointer(iptr13,ch8pte1)
  pointer(iptr14,ch8pte2)
  pointer(iptr15,ch8pte3)

  type(drvd) dpte1(nnn)
  type(drvd) dpte2(mmm,nnn)
  type(drvd) dpte3(ooo,mmm,nnn)
  integer ipte1 (nnn)
  integer ipte2 (mmm,nnn)
  integer ipte3 (ooo,mmm,nnn)
  real rpte1(nnn)
  real rpte2(mmm,nnn)
  real rpte3(ooo,mmm,nnn)
  character chpte1(nnn)
  character chpte2(mmm,nnn)
  character chpte3(ooo,mmm,nnn)
  character*8 ch8pte1(nnn)
  character*8 ch8pte2(mmm,nnn)
  character*8 ch8pte3(ooo,mmm,nnn)

  iptr1 = loc(dtarg1)
  iptr2 = loc(dtarg2)
  iptr3 = loc(dtarg3)
  iptr4 = loc(itarg1)
  iptr5 = loc(itarg2)
  iptr6 = loc(itarg3)
  iptr7 = loc(rtarg1)
  iptr8 = loc(rtarg2)
  iptr9 = loc(rtarg3)
  iptr10= loc(chtarg1)
  iptr11= loc(chtarg2)
  iptr12= loc(chtarg3)
  iptr13= loc(ch8targ1)
  iptr14= loc(ch8targ2)
  iptr15= loc(ch8targ3)


  do, i=1,n
     dpte1(i)%i1=i
     if (intne(dpte1(i)%i1, dtarg1(i)%i1)) then
        ! Error #357
        errors(357) = .true.
     endif

     dtarg1(i)%i1=2*dpte1(i)%i1
     if (intne(dpte1(i)%i1, dtarg1(i)%i1)) then
        ! Error #358
        errors(358) = .true.
     endif

     ipte1(i) = i
     if (intne(ipte1(i), itarg1(i))) then
        ! Error #359
        errors(359) = .true.
     endif

     itarg1(i) = -ipte1(i)
     if (intne(ipte1(i), itarg1(i))) then
        ! Error #360
        errors(360) = .true.
     endif

     rpte1(i) = i * 5.0
     if (realne(rpte1(i), rtarg1(i))) then
        ! Error #361
        errors(361) = .true.
     endif

     rtarg1(i) = i * (-5.0)
     if (realne(rpte1(i), rtarg1(i))) then
        ! Error #362
        errors(362) = .true.
     endif

     chpte1(i) = 'a'
     if (chne(chpte1(i), chtarg1(i))) then
        ! Error #363
        errors(363) = .true.
     endif

     chtarg1(i) = 'z'
     if (chne(chpte1(i), chtarg1(i))) then
        ! Error #364
        errors(364) = .true.
     endif

     ch8pte1(i) = 'aaaaaaaa'
     if (ch8ne(ch8pte1(i), ch8targ1(i))) then
        ! Error #365
        errors(365) = .true.
     endif

     ch8targ1(i) = 'zzzzzzzz'
     if (ch8ne(ch8pte1(i), ch8targ1(i))) then
        ! Error #366
        errors(366) = .true.
     endif

     do, j=1,m
        dpte2(j,i)%r1=1.0
        if (realne(dpte2(j,i)%r1, dtarg2(j,i)%r1)) then
           ! Error #367
           errors(367) = .true.
        endif

        dtarg2(j,i)%r1=2*dpte2(j,i)%r1
        if (realne(dpte2(j,i)%r1, dtarg2(j,i)%r1)) then
           ! Error #368
           errors(368) = .true.
        endif

        ipte2(j,i) = i
        if (intne(ipte2(j,i), itarg2(j,i))) then
           ! Error #369
           errors(369) = .true.
        endif

        itarg2(j,i) = -ipte2(j,i)
        if (intne(ipte2(j,i), itarg2(j,i))) then
           ! Error #370
           errors(370) = .true.
        endif

        rpte2(j,i) = i * (-2.0)
        if (realne(rpte2(j,i), rtarg2(j,i))) then
           ! Error #371
           errors(371) = .true.
        endif

        rtarg2(j,i) = i * (-3.0)
        if (realne(rpte2(j,i), rtarg2(j,i))) then
           ! Error #372
           errors(372) = .true.
        endif

        chpte2(j,i) = 'a'
        if (chne(chpte2(j,i), chtarg2(j,i))) then
           ! Error #373
           errors(373) = .true.
        endif

        chtarg2(j,i) = 'z'
        if (chne(chpte2(j,i), chtarg2(j,i))) then
           ! Error #374
           errors(374) = .true.
        endif

        ch8pte2(j,i) = 'aaaaaaaa'
        if (ch8ne(ch8pte2(j,i), ch8targ2(j,i))) then
           ! Error #375
           errors(375) = .true.
        endif

        ch8targ2(j,i) = 'zzzzzzzz'
        if (ch8ne(ch8pte2(j,i), ch8targ2(j,i))) then
           ! Error #376
           errors(376) = .true.
        endif
        do k=1,o
           dpte3(k,j,i)%i2(1+mod(i,5))=i
           if (intne(dpte3(k,j,i)%i2(1+mod(i,5)), &
                dtarg3(k,j,i)%i2(1+mod(i,5)))) then
              ! Error #377
              errors(377) = .true.
           endif

           dtarg3(k,j,i)%i2(1+mod(i,5))=2*dpte3(k,j,i)%i2(1+mod(i,5))
           if (intne(dpte3(k,j,i)%i2(1+mod(i,5)), &
                dtarg3(k,j,i)%i2(1+mod(i,5)))) then
              ! Error #378
              errors(378) = .true.
           endif

           ipte3(k,j,i) = i
           if (intne(ipte3(k,j,i), itarg3(k,j,i))) then
              ! Error #379
              errors(379) = .true.
           endif

           itarg3(k,j,i) = -ipte3(k,j,i)
           if (intne(ipte3(k,j,i), itarg3(k,j,i))) then
              ! Error #380
              errors(380) = .true.
           endif

           rpte3(k,j,i) = i * 2.0
           if (realne(rpte3(k,j,i), rtarg3(k,j,i))) then
              ! Error #381
              errors(381) = .true.
           endif

           rtarg3(k,j,i) = i * 3.0
           if (realne(rpte3(k,j,i), rtarg3(k,j,i))) then
              ! Error #382
              errors(382) = .true.
           endif

           chpte3(k,j,i) = 'a'
           if (chne(chpte3(k,j,i), chtarg3(k,j,i))) then
              ! Error #383
              errors(383) = .true.
           endif

           chtarg3(k,j,i) = 'z'
           if (chne(chpte3(k,j,i), chtarg3(k,j,i))) then
              ! Error #384
              errors(384) = .true.
           endif

           ch8pte3(k,j,i) = 'aaaaaaaa'
           if (ch8ne(ch8pte3(k,j,i), ch8targ3(k,j,i))) then
              ! Error #385
              errors(385) = .true.
           endif

           ch8targ3(k,j,i) = 'zzzzzzzz'
           if (ch8ne(ch8pte3(k,j,i), ch8targ3(k,j,i))) then
              ! Error #386
              errors(386) = .true.
           endif
        end do
     end do
  end do

  rtarg3 = .5
  ! Vector syntax
  do, i=1,n
     ipte3 = i
     rpte3 = rpte3+1
     do, j=1,m
        do k=1,o
           if (intne(itarg3(k,j,i), i)) then
              ! Error #387
              errors(387) = .true.
           endif

           if (realne(rtarg3(k,j,i), i+.5)) then
              ! Error #388
              errors(388) = .true.
           endif
        end do
     end do
  end do

end subroutine ptr12

! Misc
subroutine ptr13(nnn,mmm)
  common /errors/errors(400)
  logical :: errors, intne, realne, chne, ch8ne
  integer :: nnn,mmm
  integer :: i,j
  integer, parameter :: n = 9
  integer, parameter :: m = 10
  integer itarg1 (n)
  integer itarg2 (m,n)
  real rtarg1(n)
  real rtarg2(m,n)

  integer ipte1
  integer ipte2
  real rpte1
  real rpte2

  dimension ipte1(n)
  dimension rpte2(mmm,nnn)

  pointer(iptr4,ipte1)
  pointer(iptr5,ipte2)
  pointer(iptr7,rpte1)
  pointer(iptr8,rpte2)

  dimension ipte2(mmm,nnn)
  dimension rpte1(n)

  iptr4 = loc(itarg1)
  iptr5 = loc(itarg2)
  iptr7 = loc(rtarg1)
  iptr8 = loc(rtarg2)  

  do, i=1,n
     ipte1(i) = i
     if (intne(ipte1(i), itarg1(i))) then
        ! Error #389
        errors(389) = .true.
     endif

     itarg1(i) = -ipte1(i)
     if (intne(ipte1(i), itarg1(i))) then
        ! Error #390
        errors(390) = .true.
     endif

     rpte1(i) = i * 5.0
     if (realne(rpte1(i), rtarg1(i))) then
        ! Error #391
        errors(391) = .true.
     endif

     rtarg1(i) = i * (-5.0)
     if (realne(rpte1(i), rtarg1(i))) then
        ! Error #392
        errors(392) = .true.
     endif

     do, j=1,m
        ipte2(j,i) = i
        if (intne(ipte2(j,i), itarg2(j,i))) then
           ! Error #393
           errors(393) = .true.
        endif

        itarg2(j,i) = -ipte2(j,i)
        if (intne(ipte2(j,i), itarg2(j,i))) then
           ! Error #394
           errors(394) = .true.
        endif

        rpte2(j,i) = i * (-2.0)
        if (realne(rpte2(j,i), rtarg2(j,i))) then
           ! Error #395
           errors(395) = .true.
        endif

        rtarg2(j,i) = i * (-3.0)
        if (realne(rpte2(j,i), rtarg2(j,i))) then
           ! Error #396
           errors(396) = .true.
        endif

     end do
  end do
end subroutine ptr13


! Test the passing of pointers and pointees as parameters
subroutine parmtest
  integer, parameter :: n = 12
  integer, parameter :: m = 13
  integer iarray(m,n)
  pointer (ipt,iptee)
  integer iptee (m,n)

  ipt = loc(iarray)
  !  write(*,*) "loc(iarray)",loc(iarray)
  call parmptr(ipt,iarray,n,m)
  !  write(*,*) "loc(iptee)",loc(iptee)
  call parmpte(iptee,iarray,n,m)
end subroutine parmtest

subroutine parmptr(ipointer,intarr,n,m)
  common /errors/errors(400)
  logical :: errors, intne
  integer :: n,m,i,j
  integer intarr(m,n)
  pointer (ipointer,newpte)
  integer newpte(m,n)
  ! write(*,*) "loc(newpte)",loc(newpte)
  ! write(*,*) "loc(intarr)",loc(intarr) 
  ! write(*,*) "loc(newpte(1,1))",loc(newpte(1,1))
  ! newpte(1,1) = 101
  ! write(*,*) "newpte(1,1)=",newpte(1,1)
  ! write(*,*) "intarr(1,1)=",intarr(1,1)
  do, i=1,n
     do, j=1,m
        newpte(j,i) = i
        if (intne(newpte(j,i),intarr(j,i))) then
           ! Error #397
           errors(397) = .true.
        endif

        call donothing(newpte(j,i),intarr(j,i))
        intarr(j,i) = -newpte(j,i)
        if (intne(newpte(j,i),intarr(j,i))) then
           ! Error #398
           errors(398) = .true.
        endif
     end do
  end do
end subroutine parmptr

subroutine parmpte(pointee,intarr,n,m)
  common /errors/errors(400)
  logical :: errors, intne
  integer :: n,m,i,j
  integer pointee (m,n)
  integer intarr (m,n)
  !  write(*,*) "loc(pointee)",loc(pointee)
  !  write(*,*) "loc(intarr)",loc(intarr)
  !  write(*,*) "loc(pointee(1,1))",loc(pointee(1,1))
  !  pointee(1,1) = 99
  !  write(*,*) "pointee(1,1)=",pointee(1,1)
  !  write(*,*) "intarr(1,1)=",intarr(1,1)

  do, i=1,n
     do, j=1,m
        pointee(j,i) = i
        if (intne(pointee(j,i),intarr(j,i))) then
           ! Error #399
           errors(399) = .true.
        endif

        intarr(j,i) = 2*pointee(j,i)
        call donothing(pointee(j,i),intarr(j,i))
        if (intne(pointee(j,i),intarr(j,i))) then
           ! Error #400
           errors(400) = .true.
        endif
     end do
  end do
end subroutine parmpte

! Separate function calls to break Cray pointer-indifferent optimization
logical function intne(ii,jj)
  integer :: i,j
  common /foo/foo
  integer foo
  foo = foo + 1
  intne = ii.ne.jj
  if (intne) then
     write (*,*) ii," doesn't equal ",jj
  endif
end function intne

logical function realne(r1,r2)
  real :: r1, r2  
  common /foo/foo
  integer foo
  foo = foo + 1
  realne = r1.ne.r2
  if (realne) then
     write (*,*) r1," doesn't equal ",r2
  endif
end function realne

logical function chne(ch1,ch2)
  character :: ch1, ch2  
  common /foo/foo
  integer foo
  foo = foo + 1
  chne = ch1.ne.ch2
  if (chne) then
     write (*,*) ch1," doesn't equal ",ch2
  endif
end function chne

logical function ch8ne(ch1,ch2)
  character*8 :: ch1, ch2  
  common /foo/foo
  integer foo
  foo = foo + 1
  ch8ne = ch1.ne.ch2
  if (ch8ne) then
     write (*,*) ch1," doesn't equal ",ch2
  endif
end function ch8ne

subroutine donothing(ii,jj)
  common/foo/foo
  integer :: ii,jj,foo
  if (foo.le.1) then
     foo = 1
  else
     foo = foo - 1
  endif
  if (foo.eq.0) then
     ii = -1
     jj = 1
!     print *,"Test did not run correctly"
     STOP 3
  endif
end subroutine donothing

