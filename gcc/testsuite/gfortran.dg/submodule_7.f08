! { dg-do run }
!
! Example in F2008 C.8.4 to demonstrate submodules
!
module color_points
  type color_point
    private
    real :: x, y
    integer :: color
  end type color_point

  interface
! Interfaces for procedures with separate
! bodies in the submodule color_points_a
    module subroutine color_point_del ( p ) ! Destroy a color_point object
      type(color_point), allocatable :: p
    end subroutine color_point_del
! Distance between two color_point objects
    real module function color_point_dist ( a, b )
      type(color_point), intent(in) :: a, b
    end function color_point_dist
    module subroutine color_point_draw ( p ) ! Draw a color_point object
      type(color_point), intent(in) :: p
    end subroutine color_point_draw
    module subroutine color_point_new ( p ) ! Create a color_point object
      type(color_point), allocatable :: p
    end subroutine color_point_new
    module subroutine verify_cleanup ( p1, p2 ) ! Check cleanup of color_point objects
      type(color_point), allocatable :: p1, p2
    end subroutine verify_cleanup
  end interface
end module color_points

module palette_stuff
  type :: palette ;
!...
  end type palette
contains
  subroutine test_palette ( p )
! Draw a color wheel using procedures from the color_points module
    use color_points ! This does not cause a circular dependency because
! the "use palette_stuff" that is logically within
! color_points is in the color_points_a submodule.
    type(palette), intent(in) :: p
  end subroutine test_palette
end module palette_stuff


submodule ( color_points ) color_points_a ! Submodule of color_points
  integer :: instance_count = 0
  interface
! Interface for a procedure with a separate
! body in submodule color_points_b
    module subroutine inquire_palette ( pt, pal )
      use palette_stuff
! palette_stuff, especially submodules
! thereof, can reference color_points by use
! association without causing a circular
! dependence during translation because this
! use is not in the module. Furthermore,
! changes in the module palette_stuff do not
! affect the translation of color_points.
      type(color_point), intent(in) :: pt
      type(palette), intent(out) :: pal
    end subroutine inquire_palette
  end interface
contains
! Invisible bodies for public separate module procedures
! declared in the module
  module subroutine color_point_del ( p )
    type(color_point), allocatable :: p
    instance_count = instance_count - 1
    deallocate ( p )
  end subroutine color_point_del
  real module function color_point_dist ( a, b ) result ( dist )
    type(color_point), intent(in) :: a, b
    dist = sqrt( (b%x - a%x)**2 + (b%y - a%y)**2 )
  end function color_point_dist
  module subroutine color_point_new ( p )
    type(color_point), allocatable :: p
    instance_count = instance_count + 1
    allocate ( p )
! Added to example so that it does something.
    p%x = real (instance_count) * 1.0
    p%y = real (instance_count) * 2.0
    p%color = instance_count
  end subroutine color_point_new
end submodule color_points_a


submodule ( color_points:color_points_a ) color_points_b ! Subsidiary**2 submodule

contains
! Invisible body for interface declared in the ancestor module
  module subroutine color_point_draw ( p )
    use palette_stuff, only: palette
    type(color_point), intent(in) :: p
    type(palette) :: MyPalette
    call inquire_palette ( p, MyPalette )
! Added to example so that it does something.
    if (abs (p%x - real (p%color) * 1.0) .gt. 1.0e-6) STOP 1
    if (abs (p%y - real (p%color) * 2.0) .gt. 1.0e-6) STOP 2
  end subroutine color_point_draw
! Invisible body for interface declared in the parent submodule
  module procedure inquire_palette
!... implementation of inquire_palette
  end procedure inquire_palette
  module procedure verify_cleanup
    if (allocated (p1) .or. allocated (p2)) STOP 3
    if (instance_count .ne. 0) STOP 4
  end procedure
  subroutine private_stuff ! not accessible from color_points_a
!...
  end subroutine private_stuff
end submodule color_points_b


program main
  use color_points
! "instance_count" and "inquire_palette" are not accessible here
! because they are not declared in the "color_points" module.
! "color_points_a" and "color_points_b" cannot be referenced by
! use association.
  interface draw
! just to demonstrate it’s possible
    module procedure color_point_draw
  end interface
  type(color_point), allocatable :: C_1, C_2
  real :: RC
!...
  call color_point_new (c_1)
  call color_point_new (c_2)
! body in color_points_a, interface in color_points
!...
  call draw (c_1)
! body in color_points_b, specific interface
! in color_points, generic interface here.
!...
  rc = color_point_dist (c_1, c_2) ! body in color_points_a, interface in color_points
  if (abs (rc - 2.23606801) .gt. 1.0e-6) STOP 5
!...
  call color_point_del (c_1)
  call color_point_del (c_2)
! body in color_points_a, interface in color_points
  call verify_cleanup (c_1, c_2)
!...
end program main
