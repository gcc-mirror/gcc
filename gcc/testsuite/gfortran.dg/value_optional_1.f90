! { dg-do run }
! PR fortran/92887
!
! Test passing nullified/disassociated pointer or unalloc allocatable
! to OPTIONAL + VALUE

program p
  implicit none !(type, external)
  integer,      allocatable :: aa
  real,         pointer     :: pp
  character,    allocatable :: ca
  character,    pointer     :: cp
  complex,      allocatable :: za
  complex,      pointer     :: zp
  type t
     integer,      allocatable :: aa
     real,         pointer     :: pp => NULL()
     complex,      allocatable :: za
  end type t
  type(t) :: tt
  nullify (pp, cp, zp)
  call sub (aa, pp, ca, cp, za)
  call sub (tt% aa, tt% pp, z=tt% za)
  allocate (aa, pp, ca, cp, za, zp, tt% za)
  aa = 1; pp = 2.; ca = "c"; cp = "d"; za = 3.; zp = 4.; tt% za = 4.
  call ref (1,  2., "c", "d", (3.,0.))
  call ref (aa, pp, ca, cp, za)
  call val (1,  2., "c", "d", (4.,0.))
  call val (aa, pp, ca, cp, zp)
  call opt (1,  2., "c", "d", (4.,0.))
  call opt (aa, pp, ca, cp, tt% za)
  deallocate (aa, pp, ca, cp, za, zp, tt% za)
contains
  subroutine sub (x, y, c, d, z)
    integer,   value, optional :: x
    real,      value, optional :: y
    character, value, optional :: c, d
    complex,   value, optional :: z
    if (present(x)) stop 1
    if (present(y)) stop 2
    if (present(c)) stop 3
    if (present(d)) stop 4
    if (present(z)) stop 5
  end
  ! call by reference
  subroutine ref (x, y, c, d, z)
    integer   :: x
    real      :: y
    character :: c, d
    complex   :: z
    print *, "by reference  :", x, y, c, d, z
    if (x /= 1   .or. y /= 2.0) stop 11
    if (c /= "c" .or. d /= "d") stop 12
    if (z /= (3.,0.)          ) stop 13
  end
  ! call by value
  subroutine val (x, y, c, d, z)
    integer,   value :: x
    real,      value :: y
    character, value :: c, d
    complex,   value :: z
    print *, "by value      :", x, y, c, d, z
    if (x /= 1   .or. y /= 2.0) stop 21
    if (c /= "c" .or. d /= "d") stop 22
    if (z /= (4.,0.)          ) stop 23
  end
  ! call by value, optional arguments
  subroutine opt (x, y, c, d, z)
    integer,   value, optional :: x
    real,      value, optional :: y
    character, value, optional :: c, d
    complex,   value, optional :: z
    if (.not. present(x)) stop 31
    if (.not. present(y)) stop 32
    if (.not. present(c)) stop 33
    if (.not. present(d)) stop 34
    if (.not. present(z)) stop 35
    print *, "value+optional:", x, y, c, d, z
    if (x /= 1   .or. y /= 2.0) stop 36
    if (c /= "c" .or. d /= "d") stop 37
    if (z /= (4.,0.)          ) stop 38
  end
end
