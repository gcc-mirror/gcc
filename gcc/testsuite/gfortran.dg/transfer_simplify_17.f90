! { dg-do run }
! { dg-additional-options "-Wno-character-truncation" }
!
! PR fortran/128042 - simplification of TRANSFER of derived types

program pr128042
  implicit none
  type tag3
     character(4) :: c3(2)
  end type tag3
  type tag2
     type(tag3)   :: t3(2)
     character(2) :: ch2(3)
  end type tag2
  type tag1
     character(3) :: ch1
     type(tag2)   :: t2(3)
  end type tag1

  call test1 ()
  call test2 ()
  call test3 ()
  call test4 ()

contains

  ! Non-nested derived type, scalar TRANSFER result
  subroutine test1 ()
    character(*), parameter :: abc(2) = 'opqr'
    type(tag3), parameter   :: a1 = tag3(['opqr','strv'])
    type(tag3), parameter   :: a2 = tag3( 'opqr'        )
    type(tag3), parameter   :: a3 = tag3( abc(1)        )
    type(tag3), parameter   :: a4 = tag3( abc(:)        )
    type(tag3)              :: b1 = a1, b2 = a2, b3 = a3, b4 = a4

    integer,      parameter :: m  = storage_size (a1) / 8
    character(m), parameter :: u  = ""
    character(m), parameter :: s1 = transfer (a1, u)
    character(m), parameter :: s2 = transfer (a2, u)
    character(m), parameter :: s3 = transfer (a3, u)
    character(m), parameter :: s4 = transfer (a4, u)
    character(m)            :: t1, t2, t3, t4
    t1 = transfer (b1, t1)
    t2 = transfer (b2, t2)
    t3 = transfer (b3, t3)
    t4 = transfer (b4, t4)
    if (s1 /= t1) stop 1
    if (s2 /= t2) stop 2
    if (s3 /= t3) stop 3
    if (s4 /= t4) stop 4
    if (s2 /= s3) stop 5
    if (s3 /= s4) stop 6
  end subroutine test1

  ! Non-nested derived type, array TRANSFER result
  subroutine test2 ()
    type(tag3), parameter   :: u = tag3( 'opqr'        )
    type(tag3)              :: v = u
    integer,    parameter   :: m = storage_size (u) / 8
    integer                 :: j = m
    character, dimension(m) :: ww1, ww2, ww3, ww4

    ww1 = transfer(u,'*',j)    ! not simplified (j is variable)
    ww2 = transfer(v,'*',j)
    ww3 = transfer(v,'*',m)    ! not simplified (v is variable)
    ww4 = transfer(u,'*',m)    ! this was mis-simplified

    if (any (ww1 /= ww2)) stop 11
    if (any (ww1 /= ww3)) stop 12
    if (any (ww1 /= ww4)) stop 13
  end subroutine test2

  ! Nested derived types, simple case
  subroutine test3 ()
    type(tag2), parameter   :: x = tag2(tag3('opqr'),'12')
    type(tag2)              :: y = x
    integer,    parameter   :: n = storage_size (x) / 8
    integer                 :: k = n
    character, dimension(n) :: zz1, zz2, zz3, zz4

    zz1 = transfer(x,'*',k)    ! not simplified (k is variable)
    zz2 = transfer(y,'*',k)
    zz3 = transfer(y,'*',n)    ! not simplified (y is variable)
    zz4 = transfer(x,'*',n)    ! this was mis-simplified

    if (any (zz1 /= zz2)) stop 14
    if (any (zz1 /= zz3)) stop 15
    if (any (zz1 /= zz4)) stop 16
  end subroutine test3

  ! Derived from original test in PR
  subroutine test4 ()
    type(tag1) :: sourceTAG(2)
    parameter (sourceTAG = &
                  (/tag1('abc',                                         &
                         tag2((/tag3('klmn'),tag3((/'opqr','strv'/))/), &
                              (/'ef','gh','ij'/))),                     &
                    tag1('xyz',tag2(tag3('6789'),'123')) /) )

    character(10) :: moldCH, rcCH(10), rrCH(10)
    parameter (moldCH = ' ')
    parameter (rcCH = transfer (sourceTAG,moldCH,10))
    integer :: i

    rrCH = transfer (sourceTAG,moldCH,10)

    do i=1,10
       call checkch(rcCH(i),rrCH(i))
    enddo

  end subroutine test4

  subroutine checkch(ch,cht)
    character(*) ch,cht
    if (ch /= cht) then
       print *,len(ch),len(cht),len_trim(ch),len_trim(cht)
       print *,ch
       print *,cht
       print *,'??? ng ???'
       stop 99
    endif
  end subroutine checkch

end
