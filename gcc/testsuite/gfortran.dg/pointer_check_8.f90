! { dg-do compile }
! { dg-options "-fcheck=pointer" }
!
! PR 46809: [OOP] ICE with -fcheck=pointer for CLASS IS
!
! Contributed by Salvatore Filippone <sfilippone@uniroma2.it>

  type t
  end type t

contains

  subroutine sub(a)
    class(t) :: a
    select type (a)
      class is (t)
        print *, 'Hi there'
    end select
  end subroutine

end
