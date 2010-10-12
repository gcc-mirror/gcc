! { dg-do run }
! { dg-options "-std=f2003 -fall-intrinsics" }

! PR fortran/38936
! Check associate to polymorphic entities.

! Contributed by Tobias Burnus, burnus@gcc.gnu.org.

type t
end type t

type, extends(t) :: t2
end type t2

class(t), allocatable :: a, b
allocate( t :: a)
allocate( t2 :: b)

associate ( one => a, two => b)
  select type(two)
    type is (t)
      call abort ()
    type is (t2)
      print *, 'OK', two
    class default
      call abort ()
  end select
  select type(one)
    type is (t2)
      call abort ()
    type is (t)
      print *, 'OK', one
    class default
      call abort ()
  end select
end associate
end
