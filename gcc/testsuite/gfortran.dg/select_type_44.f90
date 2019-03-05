! { dg-do run }
!
! Test the fix for PR87566
!
! Contributed by Antony Lewis  <antony@cosmologist.info>
!
  call AddArray
contains
  subroutine AddArray()
    type Object_array_pointer
        class(*), pointer :: p(:) => null()
    end type Object_array_pointer
    class(*), pointer :: Pt => null()
    type (Object_array_pointer) :: obj
    character(3), target :: tgt1(2) = ['one','two']
    character(5), target :: tgt2(2) = ['three','four ']

    allocate (Pt, source = Object_array_pointer ())
    select type (Pt)
      type is (object_array_pointer)
        Pt%p => tgt1
    end select

    select type (Pt)
      class is (object_array_pointer)
        select type (Point=> Pt%P)
          type is (character(*))
            if (any (Point .ne. tgt1)) stop 1
            Point = ['abc','efg']
        end select
    end select

    select type (Pt)
      class is (object_array_pointer)
        select type (Point=> Pt%P)
          type is (character(*))
            if (any (Point .ne. ['abc','efg'])) stop 2
        end select
    end select

  end subroutine AddArray
end
