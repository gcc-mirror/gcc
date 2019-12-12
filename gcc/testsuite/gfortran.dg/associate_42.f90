! { dg-do run }
!
! Tests the fix for a bug that was found in the course of fixing PR87566.
!
! Contributed by Paul Thomas  <pault@gcc.gnu.org>
!
    call AddArray
contains
  subroutine AddArray()
    type Object_array_pointer
        class(*), pointer :: p(:) => null()
    end type Object_array_pointer

    type (Object_array_pointer) :: obj
    character(3), target :: tgt1(2) = ['one','two']
    character(5), target :: tgt2(2) = ['three','four ']
    real, target :: tgt3(3) = [1.0,2.0,3.0]

    obj%p => tgt1
    associate (point => obj%p)
      select type (point)         ! Used to ICE here.
        type is (character(*))
          if (any (point .ne. tgt1)) stop 1
      end select
      point => tgt2
    end associate

    select type (z => obj%p)
      type is (character(*))
        if (any (z .ne. tgt2)) stop 2
    end select

    obj%p => tgt3
    associate (point => obj%p)
      select type (point)
        type is (real)
          if (any (point .ne. tgt3)) stop 3
      end select
    end associate
  end subroutine AddArray
end
