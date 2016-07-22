! { dg-do compile }

module m
  integer :: i
end module m
subroutine f1
  type t
    integer :: i
  end type t
  interface
    integer function f3 (a, b)
      !$omp declare simd (f3) uniform (a)
      use m
      import :: t
      implicit none
      type (t) :: a
      integer :: b
    end function f3
  end interface
  interface
    integer function f4 (a, b)
      use m
      !$omp declare simd (f4) uniform (a)
      import :: t
      implicit none
      type (t) :: a
      integer :: b
    end function f4
  end interface
  interface
    integer function f5 (a, b)
      use m
      import :: t
      !$omp declare simd (f5) uniform (a)
      implicit none
      type (t) :: a
      integer :: b
    end function f5
  end interface
  interface
    integer function f6 (a, b)
      use m
      import :: t
      implicit none
      !$omp declare simd (f6) uniform (a)
      type (t) :: a
      integer :: b
    end function f6
  end interface
  interface
    integer function f7 (a, b)
      use m
      import :: t
      implicit none
      type (t) :: a
      !$omp declare simd (f7) uniform (a)
      integer :: b
    end function f7
  end interface
  call f2
contains
  subroutine f2
    !$omp threadprivate (t1)
    use m
    !$omp threadprivate (t2)
    implicit none
    !$omp threadprivate (t3)
    integer, save :: t1, t2, t3, t4
    !$omp threadprivate (t4)
    t1 = 1; t2 = 2; t3 = 3; t4 = 4
  end subroutine f2
  subroutine f8
    !$omp declare reduction (f8_1:real:omp_out = omp_out + omp_in)
    use m
    !$omp declare reduction (f8_2:real:omp_out = omp_out + omp_in)
    implicit none
    !$omp declare reduction (f8_3:real:omp_out = omp_out + omp_in)
    integer :: j
    !$omp declare reduction (f8_4:real:omp_out = omp_out + omp_in)
  end subroutine f8
  subroutine f9
    !$omp declare target (f9_1)
    use m
    !$omp declare target (f9_2)
    implicit none
    !$omp declare target (f9_3)
    !$omp declare target
    integer, save :: f9_1, f9_2, f9_3, f9_4
    !$omp declare target (f9_4)
    f9_1 = 1; f9_2 = 2; f9_3 = 3; f9_4 = 4
  end subroutine f9
end subroutine f1
