! { dg-do compile }

module m
  integer :: i
end module m
subroutine f1
  call f2
contains
  subroutine f2
    use m
    implicit none
    integer, save :: t
    t = 1
    !$omp threadprivate (t1)	! { dg-error "\\!\\\$OMP THREADPRIVATE statement at \\(1\\) cannot appear after executable statements" }
  end subroutine f2
  subroutine f3
    use m
    implicit none
    integer :: j
    j = 1
    !$omp declare reduction (foo:real:omp_out = omp_out + omp_in)	! { dg-error "\\!\\\$OMP DECLARE REDUCTION statement at \\(1\\) cannot appear after executable statements" }
  end subroutine f3
  subroutine f4
    use m
    implicit none
    !$omp declare target
    integer, save :: f4_1
    f4_1 = 1
    !$omp declare target (f4_1)	! { dg-error "\\!\\\$OMP DECLARE TARGET statement at \\(1\\) cannot appear after executable statements" }
    !$omp declare target	! { dg-error "\\!\\\$OMP DECLARE TARGET statement at \\(1\\) cannot appear after executable statements" }
  end subroutine f4
  integer function f5 (a, b)
    integer :: a, b
    a = 1; b = 2
    !$omp declare simd (f5) notinbranch	! { dg-error "\\!\\\$OMP DECLARE SIMD statement at \\(1\\) cannot appear after executable statements" }
  end function f5
end subroutine f1
