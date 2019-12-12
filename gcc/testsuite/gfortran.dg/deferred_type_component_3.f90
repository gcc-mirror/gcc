! { dg-do run }
!
! Test the fix for PR87151 by exercising deferred length character
! array components.
!
! Based on the contribution by Valery Weber <valeryweber@hotmail.com>
!
module bvec
    type, public :: bvec_t
     private
     character(:), dimension(:), allocatable :: vc
   contains
     PROCEDURE, PASS :: create
     PROCEDURE, PASS :: test_bvec
     PROCEDURE, PASS :: delete
  end type bvec_t
contains
  subroutine create (this, switch)
    class(bvec_t), intent(inout) :: this
    logical :: switch
    if (switch) then
      allocate (character(2)::this%vc(3))
      if (len (this%vc) .ne. 2) stop 1     ! The orignal problem. Gave 0.

! Check that reallocation on assign does what it should do as required by
! F2003 7.4.1.3. ie. reallocation occurs because LEN has changed.
      this%vc = ['abcd','efgh','ijkl']
    else
      allocate (this%vc, source = ['abcd','efgh','ijkl'])
    endif
  end subroutine create

  subroutine test_bvec (this)
    class(bvec_t), intent(inout) :: this
    character(20) :: buffer
    if (allocated (this%vc)) then
      if (len (this%vc) .ne. 4) stop 2
      if (size (this%vc) .ne. 3) stop 3
! Check array referencing and scalarized array referencing
      if (this%vc(2) .ne. 'efgh') stop 4
      if (any (this%vc .ne. ['abcd','efgh','ijkl'])) stop 5
! Check full array io
      write (buffer, *) this%vc
      if (trim (buffer(2:)) .ne. 'abcdefghijkl') stop 6
! Make sure that substrings work correctly
      write (buffer, *) this%vc(:)(2:3)
      if (trim (buffer(2:)) .ne. 'bcfgjk') stop 7
      write (buffer, *) this%vc(2:)(2:3)
      if (trim (buffer(2:)) .ne. 'fgjk') stop 8
    endif
  end subroutine test_bvec

  subroutine delete (this)
    class(bvec_t), intent(inout) :: this
    if (allocated (this%vc)) then
      deallocate (this%vc)
    endif
  end subroutine delete
end module bvec

program test
  use bvec
  type(bvec_t) :: a
  call a%create (.false.)
  call a%test_bvec
  call a%delete

  call a%create (.true.)
  call a%test_bvec
  call a%delete
end program test
