! { dg-do run }
!
! Tests the fix for PR67674
!
! Contributed by Kristopher Kuhlman  <kristopher.kuhlman@gmail.com>
!
program test
  implicit none

  type string_type
    character(len=:), allocatable :: name
  end type string_type
  type(string_type), allocatable :: my_string_type

  allocate(my_string_type)
  allocate(character(len=0) :: my_string_type%name)

!  print *, 'length main program before',len(my_string_type%name)

  call inputreadword1(my_string_type%name)

!  print *, 'length main program after',len(my_string_type%name)
!  print *, 'final result:',my_string_type%name
  if (my_string_type%name .ne. 'here the word is finally set') STOP 1

contains
  subroutine inputreadword1(word_intermediate)
    character(len=:), allocatable :: word_intermediate

!    print *, 'length intermediate before',len(word_intermediate)
    call inputreadword2(word_intermediate)
!    print *, 'length intermediate after',len(word_intermediate)
!    print *, word_intermediate

  end subroutine inputreadword1

  subroutine inputreadword2(word)
    character(len=:), allocatable :: word

!    print *, 'length inner before',len(word)
    word = 'here the word is finally set' ! want automatic reallocation to happen here
!    print *, 'length inner after',len(word)
!    print *, word

  end subroutine inputreadword2
end program test
