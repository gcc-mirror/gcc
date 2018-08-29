! { dg-do run }
! { dg-options "-std=gnu" }
! Tests the fix for PR29786, in which initialization of overlapping
! equivalence elements caused a compile error.
!
! Contributed by Bernhard Fischer <aldot@gcc.gnu.org>
!
block data
  common /global/ ca (4)
  integer(4) ca, cb
  equivalence (cb, ca(3))
  data (ca(i), i = 1, 2) /42,43/, ca(4) /44/
  data cb /99/
end block data

  integer(4), parameter :: abcd = ichar ("a") + 256_4 * (ichar("b") + 256_4 * &
                                 (ichar ("c") + 256_4 * ichar ("d")))
  logical(4), parameter :: bigendian = transfer (abcd, "wxyz") .eq. "abcd"

  call int4_int4
  call real4_real4
  call complex_real
  call check_block_data
  call derived_types         ! Thanks to Tobias Burnus for this:)
!
! This came up in PR29786 comment #9 - Note the need to treat endianess
! Thanks Dominique d'Humieres:)
!
  if (bigendian) then
    if (d1mach_little (1) .ne. transfer ((/0_4, 1048576_4/), 1d0)) STOP 1
    if (d1mach_little (2) .ne. transfer ((/-1_4,2146435071_4/), 1d0)) STOP 2
  else
    if (d1mach_big (1) .ne. transfer ((/1048576_4, 0_4/), 1d0)) STOP 3
    if (d1mach_big (2) .ne. transfer ((/2146435071_4,-1_4/), 1d0)) STOP 4
  end if 
!
contains
  subroutine int4_int4
      integer(4)         a(4)
      integer(4)         b
      equivalence (b,a(3))
      data b/3/
      data (a(i), i=1,2) /1,2/, a(4) /4/
      if (any (a .ne. (/1, 2, 3, 4/))) STOP 5
  end subroutine int4_int4
  subroutine real4_real4
      real(4)         a(4)
      real(4)         b
      equivalence (b,a(3))
      data b/3.0_4/
      data (a(i), i=1,2) /1.0_4, 2.0_4/, &
            a(4) /4.0_4/
      if (sum (abs (a -  &
          (/1.0_4, 2.0_4, 3.0_4, 4.0_4/))) > 1.0e-6) STOP 6
  end subroutine real4_real4
  subroutine complex_real
      complex(4)         a(4)
      real(4)            b(2)
      equivalence (b,a(3))
      data b(1)/3.0_4/, b(2)/4.0_4/
      data (a(i), i=1,2) /(0.0_4, 1.0_4),(2.0_4,0.0_4)/, &
            a(4) /(0.0_4,5.0_4)/
      if (sum (abs (a - (/(0.0_4, 1.0_4),(2.0_4, 0.0_4), &
          (3.0_4, 4.0_4),(0.0_4, 5.0_4)/)))  > 1.0e-6) STOP 7
  end subroutine complex_real
  subroutine check_block_data
      common /global/ ca (4)
      equivalence (ca(3), cb)
      integer(4) ca
      if (any (ca .ne. (/42, 43, 99, 44/))) STOP 8
  end subroutine check_block_data
  function d1mach_little(i) result(d1mach)
    implicit none
    double precision d1mach,dmach(5)
    integer i
    integer*4 large(4),small(4)
    equivalence ( dmach(1), small(1) )
    equivalence ( dmach(2), large(1) )
    data small(1),small(2) / 0,   1048576/
    data large(1),large(2) /-1,2146435071/
    d1mach = dmach(i) 
  end function d1mach_little
  function d1mach_big(i) result(d1mach)
    implicit none
    double precision d1mach,dmach(5)
    integer i
    integer*4 large(4),small(4)
    equivalence ( dmach(1), small(1) )
    equivalence ( dmach(2), large(1) )
    data small(1),small(2) /1048576,    0/
    data large(1),large(2) /2146435071,-1/
    d1mach = dmach(i) 
  end function d1mach_big
    subroutine derived_types
      TYPE T1
        sequence
        character (3) :: chr
        integer :: i = 1
        integer :: j
        END TYPE T1
      TYPE T2
        sequence
        character (3) :: chr = "wxy"
        integer :: i = 1
        integer :: j = 4
      END TYPE T2
      TYPE(T1) :: a1
      TYPE(T2) :: a2
      EQUIVALENCE(a1,a2)         ! { dg-warning="mixed|components" }
      if (a1%chr .ne. "wxy") STOP 9
      if (a1%i .ne. 1) STOP 10
      if (a1%j .ne. 4) STOP 11
      end subroutine derived_types
end
