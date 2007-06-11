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

  call int4_int4
  call real4_real4
  call complex_real
  call check_block_data
  call derived_types         ! Thanks to Tobias Burnus for this:)
!
! This came up in PR29786 comment #9
!
  if (d1mach (1) .ne. transfer ((/0_4, 1048576_4/), 1d0)) call abort ()
  if (d1mach (2) .ne. transfer ((/-1_4,2146435071_4/), 1d0)) call abort ()
!
contains
  subroutine int4_int4
      integer(4)         a(4)
      integer(4)         b
      equivalence (b,a(3))
      data b/3/
      data (a(i), i=1,2) /1,2/, a(4) /4/
      if (any (a .ne. (/1, 2, 3, 4/))) call abort ()
  end subroutine int4_int4
  subroutine real4_real4
      real(4)         a(4)
      real(4)         b
      equivalence (b,a(3))
      data b/3.0_4/
      data (a(i), i=1,2) /1.0_4, 2.0_4/, &
            a(4) /4.0_4/
      if (sum (abs (a -  &
          (/1.0_4, 2.0_4, 3.0_4, 4.0_4/))) > 1.0e-6) call abort ()
  end subroutine real4_real4
  subroutine complex_real
      complex(4)         a(4)
      real(4)            b(2)
      equivalence (b,a(3))
      data b(1)/3.0_4/, b(2)/4.0_4/
      data (a(i), i=1,2) /(0.0_4, 1.0_4),(2.0_4,0.0_4)/, &
            a(4) /(0.0_4,5.0_4)/
      if (sum (abs (a - (/(0.0_4, 1.0_4),(2.0_4, 0.0_4), &
          (3.0_4, 4.0_4),(0.0_4, 5.0_4)/)))  > 1.0e-6) call abort ()
  end subroutine complex_real
  subroutine check_block_data
      common /global/ ca (4)
      equivalence (ca(3), cb)
      integer(4) ca
      if (any (ca .ne. (/42, 43, 99, 44/))) call abort ()
  end subroutine check_block_data
  function d1mach(i)
    implicit none
    double precision d1mach,dmach(5)
    integer i,large(4),small(4)
    equivalence ( dmach(1), small(1) )
    equivalence ( dmach(2), large(1) )
    data small(1),small(2) / 0,   1048576/
    data large(1),large(2) /-1,2146435071/
    d1mach = dmach(i) 
  end function d1mach
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
      if (a1%chr .ne. "wxy") call abort ()
      if (a1%i .ne. 1) call abort ()
      if (a1%j .ne. 4) call abort ()
      end subroutine derived_types
end
