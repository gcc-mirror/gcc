! { dg-do compile }
! PR 96843 - this was wrongly rejected.
! Test case by William Clodius.

module test_shape_mismatch
! Implements zero based bitsets of size up to HUGE(0_INT32).
! The current code uses 32 bit integers to store the bits and uses all 32 bits.
! The code assumes two's complement integers, and treats negative integers as
! having the sign bit set.

    use, intrinsic ::            &
        iso_fortran_env, only:   &
            bits_kind  => int32, &
            block_kind => int64, &
            int8,                &
            dp => real64

    implicit none

    private

    integer, parameter ::                                                     &
        block_size  = bit_size(0_block_kind),                                 &
        block_shift = int( ceiling( log( real(block_size, dp) )/log(2._dp) ) )

    public :: bits_kind
! Public constant

    public :: bitset_t
! Public type

    public ::          &
        assignment(=)

    type, abstract :: bitset_t
        private
        integer(bits_kind) :: num_bits

    end type bitset_t


    type, extends(bitset_t) :: bitset_large
        private
        integer(block_kind), private, allocatable :: blocks(:)

    end type bitset_large

    interface assign

        pure module subroutine assign_log8_large( self, alogical )
!!     Used to define assignment from an array of type LOG for bitset_t
            type(bitset_large), intent(out) :: self
            logical(int8), intent(in) :: alogical(:)
        end subroutine assign_log8_large

    end interface assign

contains

    pure module subroutine assign_log8_large( self, alogical )
!     Used to define assignment from an array of type LOG for bitset_t
        type(bitset_large), intent(out) :: self
        logical(int8), intent(in)  :: alogical(:)

        integer(bits_kind) :: blocks
        integer(bits_kind) :: log_size
        integer(bits_kind) :: index

        log_size = size( alogical, kind=bits_kind )
        self % num_bits = log_size
        if ( log_size == 0 ) then
            blocks = 0

        else
            blocks = (log_size-1)/block_size + 1

        end if
        allocate( self % blocks( blocks ) )
        self % blocks(:) = 0

        return
    end subroutine assign_log8_large

end module test_shape_mismatch
