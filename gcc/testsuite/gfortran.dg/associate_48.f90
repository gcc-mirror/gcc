! { dg=do run }
!
! Test the fix for PR90498.
!
! Contributed by Vladimir Fuka  <vladimir.fuka@gmail.com>
!
  type field_names_a
    class(*), pointer :: var(:) =>null()
  end type

  type(field_names_a),pointer :: a(:)
  allocate (a(2))

  allocate (a(1)%var(2), source = ["hello"," vlad"])
  allocate (a(2)%var(2), source = ["HELLO"," VLAD"])
  call s(a)
  deallocate (a(1)%var)
  deallocate (a(2)%var)
  deallocate (a)
contains
  subroutine s(a)

    type(field_names_a) :: a(:)

    select type (var => a(1)%var)
      type is (character(*))
        if (any (var .ne. ["hello"," vlad"])) stop 1
      class default
        stop
    end select

    associate (var => a(2)%var)
      select type (var)
        type is (character(*))
          if (any (var .ne. ["HELLO"," VLAD"])) stop 2
        class default
          stop
      end select
    end associate
  end
end
