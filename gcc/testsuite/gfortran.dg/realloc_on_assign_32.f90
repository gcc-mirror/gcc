! { dg-do run }
!
! Test the fix for PR88980 in which the 'span' field if the descriptor
! for 'Items' was not set, causing the assignment to segfault.
!
! Contributed by Antony Lewis  <antony@cosmologist.info>
!
program tester
  call gbug
contains
  subroutine gbug
    type TNameValue
      character(LEN=:), allocatable :: Name
    end type TNameValue

    type TNameValue_pointer
      Type(TNameValue), allocatable :: P
    end type TNameValue_pointer

    Type TType
      type(TNameValue_pointer), dimension(:), allocatable :: Items
    end type TType
    Type(TType) T

    allocate(T%Items(2))
    allocate(T%Items(2)%P)
    T%Items(2)%P%Name =  'test'
    if (T%Items(2)%P%Name .ne.  'test') stop 1

  end subroutine gbug
end program tester
