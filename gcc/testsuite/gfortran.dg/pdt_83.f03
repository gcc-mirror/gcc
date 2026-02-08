! { dg-do run }
!
! Test the fix for pr123673, which caused the errors below. Although some of thses errors are
! not checked here, it has been verified that they are fixed by the patch for the main fault.
!
! Contributed by Damian Rouson  <damian@archaeologic.codes>
!
module input_output_pair_m
  implicit none

  ! Moving the contents of this module to the main program caused several 
  ! compile-time errors that do not occur with other brands.

  type tensor_t(k)
    integer, kind :: k = kind(1.)
    real(k), allocatable :: values_(:)
  end type

  type input_output_pair_t(k)
    integer, kind :: k = kind(1.)
    type(tensor_t(k)) inputs_, expected_outputs_
  end type

contains

  ! Moving just the function below to become an internal subprogram in the main program
  ! caused similar compile-time errors to those mentioned above

  type(input_output_pair_t) elemental function input_output_pair(inputs, expected_outputs)
    type(tensor_t), intent(in) :: inputs, expected_outputs
    input_output_pair%inputs_ = inputs
    input_output_pair%expected_outputs_ = expected_outputs
  end function

end module

program trainable_network_test
  use input_output_pair_m
  implicit none

  type bin_t
    integer  first_, last_
  end type

  ! Removing the mini_batch_t's all instances of 'k' below caused
  ! the following compile-time error on the above 'use' statement:
  ! "Cannot convert TYPE(input_output_pair_t) to TYPE(Pdtinput_output_pair_t_4) at (1)",
  ! where "1" is positiioned just after 'use'

  type mini_batch_t(k)
    integer, kind :: k = kind(1.)
    type(input_output_pair_t(k)), allocatable :: input_output_pairs_(:)
  end type

  type(input_output_pair_t), allocatable :: input_output_pairs(:)
  type(bin_t), allocatable :: bins(:)
  type(mini_batch_t) mini_batch_1
  integer, parameter :: num_pairs = 10 ! 7 is the mininum value that causes segmentation fault
  integer, parameter :: n_bins = 5     ! 2 is the mininum value that causes segmentation fault
  integer p, b
  
  input_output_pairs = input_output_pair( &
                       [(tensor_t([real (p, kind (1.0)), &
                                   real (p *10, kind (1.0))]), p = 1, num_pairs)], &
                       [(tensor_t([real (p *20, kind (1.0)), &
                                   real (p *30, kind (1.0))]), p = 1, num_pairs)])
  bins = [(bin(num_pairs, n_bins, b), b = 1, n_bins)]

  ! The assignment statement below caused a segmentation fault with gfortran.
  ! Converting the assignment to an 'associate' statement also caused a seg fault.

  mini_batch_1 = mini_batch(input_output_pairs(bins(n_bins)%first_:bins(n_bins)%last_))

  if (any (mini_batch_1%input_output_pairs_(bins(1)%first_)%inputs_%values_ /= [9.0, 90.0])) stop 1
  if (any (mini_batch_1%input_output_pairs_(bins(1)%last_)%inputs_%values_ /= [10.0, 100.0])) stop 2

  associate (mini_batch_2 => &
             mini_batch(input_output_pairs(bins(n_bins-1)%first_:bins(n_bins-1)%last_)))
    if (any (mini_batch_2%input_output_pairs_(bins(1)%first_)%inputs_%values_ /= [7.0, 70.0])) stop 3
    if (any (mini_batch_2%input_output_pairs_(bins(1)%last_)%inputs_%values_ /= [8.0, 80.0])) stop 4
  end associate

  deallocate (bins, input_output_pairs, mini_batch_1%input_output_pairs_)

contains

  type(bin_t) function bin(num_items, num_bins, bin_number)
    integer num_items, num_bins, bin_number
    associate(remainder => mod(num_items, num_bins), items_per_bin => num_items/num_bins)
      if (bin_number <= remainder) then
        bin%first_ = 1 + (bin_number-1)*(items_per_bin+1)
        bin%last_  = bin_number*(items_per_bin+1)
      else
        bin%first_ = 1 + (remainder-1)*(items_per_bin+1) + 1 + (bin_number-remainder)*items_per_bin
        bin%last_ = remainder*(items_per_bin+1) + (bin_number-remainder)*items_per_bin
      end if
    end associate
  end function

  type(mini_batch_t) function mini_batch(input_output_pairs)
    type(input_output_pair_t) input_output_pairs(:)
    mini_batch%input_output_pairs_ = input_output_pairs
  end function

end program
