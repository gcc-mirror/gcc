! { dg-do compile }
!
! Contributed by Vladimir Nikishkin  <lockywolf@gmail.com>
!
module test
  type testtype
     class(*), allocatable :: t
  end type testtype
contains
  subroutine testproc( x )
    class(testtype) :: x
    associate ( temp => x%t)
      select type (temp)
         type is (integer)
      end select
    end associate
  end subroutine testproc
end module test
