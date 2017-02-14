! { dg-do run }
!
! Contributed by Nathanael Huebbe
! Check fix for PR/70842

program foo

  TYPE, ABSTRACT :: t_Intermediate
  END TYPE t_Intermediate

  type, extends(t_Intermediate) :: t_Foo
    character(:), allocatable :: string
  end type t_Foo

  class(t_Foo), allocatable :: obj

  allocate(obj)
  obj%string = "blabarfoo"

  call bar(obj)

  deallocate(obj)
contains
  subroutine bar(me)
    class(t_Intermediate), target :: me

    class(*), pointer :: alias

    select type(me)
      type is(t_Foo)
      if (len(me%string) /= 9) call abort()
    end select

    alias => me
    select type(alias)
      type is(t_Foo)
        if (len(alias%string) /= 9) call abort()
    end select
  end subroutine bar
end program foo

