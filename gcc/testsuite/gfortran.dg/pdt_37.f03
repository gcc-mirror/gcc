! { dg-do compile }
!
! Tests the fixes for PR82943.
!
! This test focuses on the errors produced by incorrect LEN parameters for dummy
! arguments of PDT Typebound Procedures.
!
! Contributed by Alexander Westbrooks  <ctechnodev@gmail.com>
!
module test_len_param
   implicit none
   type :: param_deriv_type(a)
       integer, len :: a
   contains
       procedure :: assumed_len_param           ! Good. No error expected.
       procedure :: assumed_len_param_ptr       ! { dg-error "must not be POINTER" }
       procedure :: assumed_len_param_alloc     ! { dg-error "must not be ALLOCATABLE" }
       procedure :: deferred_len_param          ! { dg-error "must be ASSUMED" }
       procedure :: deferred_len_param_ptr      ! { dg-error "must be ASSUMED" }
       procedure :: deferred_len_param_alloc    ! { dg-error "must be ASSUMED" }
       procedure :: fixed_len_param             ! { dg-error "must be ASSUMED" }
       procedure :: fixed_len_param_ptr         ! { dg-error "must be ASSUMED" }
       procedure :: fixed_len_param_alloc       ! { dg-error "must be ASSUMED" }

   end type

contains
    subroutine assumed_len_param(this)
       class(param_deriv_type(*)), intent(inout) :: this                            ! Good. No error expected.
    !    TYPE(param_deriv_type(*)), intent(inout) :: that                           ! Good. No error expected.
    end subroutine

    subroutine assumed_len_param_ptr(this, that)
        class(param_deriv_type(*)), intent(inout), pointer :: this                  ! Good. No error expected.
        TYPE(param_deriv_type(*)), intent(inout), allocatable :: that               ! Good. No error expected.
    end subroutine

    subroutine assumed_len_param_alloc(this, that)
        class(param_deriv_type(*)), intent(inout), allocatable :: this              ! Good. No error expected.
        TYPE(param_deriv_type(*)), intent(inout), allocatable :: that               ! Good. No error expected.
    end subroutine

    subroutine deferred_len_param(this, that)                                       ! { dg-error "requires either the POINTER or ALLOCATABLE attribute" }
        class(param_deriv_type(:)), intent(inout) :: this
        TYPE(param_deriv_type(:)), intent(inout) :: that                            ! Good. No error expected.
    end subroutine

    subroutine deferred_len_param_ptr(this, that)
        class(param_deriv_type(:)), intent(inout), pointer :: this                  ! Good. No error expected.
        TYPE(param_deriv_type(:)), intent(inout), pointer :: that                   ! Good. No error expected.
    end subroutine

    subroutine deferred_len_param_alloc(this, that)
        class(param_deriv_type(:)), intent(inout), allocatable :: this              ! Good. No error expected.
        TYPE(param_deriv_type(:)), intent(inout), allocatable :: that               ! Good. No error expected.
    end subroutine

    subroutine fixed_len_param(this, that)
        class(param_deriv_type(10)), intent(inout) :: this                          ! Good. No error expected.
        TYPE(param_deriv_type(10)), intent(inout) :: that                           ! Good. No error expected.
    end subroutine

    subroutine fixed_len_param_ptr(this, that)
        class(param_deriv_type(10)), intent(inout), pointer :: this                 ! Good. No error expected.
        TYPE(param_deriv_type(10)), intent(inout), pointer :: that                  ! Good. No error expected.
    end subroutine

    subroutine fixed_len_param_alloc(this, that)
        class(param_deriv_type(10)), intent(inout), allocatable :: this             ! Good. No error expected.
        TYPE(param_deriv_type(10)), intent(inout), allocatable :: that              ! Good. No error expected.
    end subroutine

end module

