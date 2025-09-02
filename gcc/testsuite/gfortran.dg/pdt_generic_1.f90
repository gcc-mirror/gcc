! { dg-do run }
!
! Check the fix for pr121398
!
! Contributed by Damian Rouson  <damian@archaeologic.codes>
!
module tensor_m
  implicit none
  private
  public tensor_t

  type tensor_t(k)
    integer, kind :: k
    integer :: n
  contains
    procedure, private :: default_real_num_components
    procedure, private :: default_real_num_components2
    procedure, private ::  double_precision_num_components
    procedure, private, pass(self) ::  quad_precision_num_components
    generic :: num_components => default_real_num_components, &   ! Failed ambiguity test
                                 default_real_num_components2, &
                                 double_precision_num_components, &
                                 quad_precision_num_components
  end type

  interface

    module function default_real_num_components(self) result(res)
      implicit none
      class(tensor_t(kind(0.))) self
      integer :: res
    end function

    module function default_real_num_components2(self, another) result(res)
      implicit none
      class(tensor_t(kind(0.))) self, another
      integer :: res
    end function

    module function double_precision_num_components(self) result(res)
      implicit none
      class(tensor_t(kind(0.0_8))) self
      integer :: res
    end function

    module function quad_precision_num_components(l, self) result(res)
      implicit none
      class(tensor_t(kind(0.0_16))) self
      integer :: l
      integer :: res
    end function

  end interface

end module 

submodule (tensor_m) tensor_m_components
contains
    module procedure default_real_num_components
      implicit none
      self%n = 10
      res = 1
    end

    module procedure default_real_num_components2
      implicit none
      self%n = 2 * another%n
      res = 1
    end

    module procedure double_precision_num_components
      implicit none
      self%n = 20
      res = 2
    end

    module procedure quad_precision_num_components
      implicit none
      self%n = 10 * l
      res = l
    end
end

    use tensor_m
    type (tensor_t(kind(0.))) :: a
    type (tensor_t(kind(0.))) :: ap
    type (tensor_t(kind(0.0_8))) :: b
    type (tensor_t(kind(0.0_16))) :: c
    if (a%num_components () /= 1) stop 1
    if (ap%num_components (a) /= 1) stop 2
    if (2 * a%n /= ap%n) stop 3
    if (b%num_components () /= 2 ) stop 4
    if (c%num_components (42) /= 42 ) stop 5
end
