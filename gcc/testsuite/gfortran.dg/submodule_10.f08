! { dg-do compile }
! { dg-require-visibility "" }
!
! Checks that PRIVATE enities are visible to submodules.
!
! Contributed by Salvatore Filippone  <salvatore.filippone@uniroma2.it>
!
module const_mod
  integer, parameter  :: ndig=8
  integer, parameter  :: ipk_ = selected_int_kind(ndig)
  integer, parameter  :: longndig=12
  integer, parameter  :: long_int_k_ = selected_int_kind(longndig)
  integer, parameter  :: mpik_ = kind(1)

  integer(ipk_), parameter, public :: success_=0

end module const_mod


module error_mod
  use const_mod

  integer(ipk_), parameter, public :: act_ret_=0
  integer(ipk_), parameter, public :: act_print_=1
  integer(ipk_), parameter, public :: act_abort_=2

  integer(ipk_), parameter, public ::  no_err_ = 0

  public error, errcomm, get_numerr, &
       & error_handler, &
       & ser_error_handler, par_error_handler


  interface error_handler
    module subroutine ser_error_handler(err_act)
      integer(ipk_), intent(inout) ::  err_act
    end subroutine ser_error_handler
    module subroutine par_error_handler(ictxt,err_act)
      integer(mpik_), intent(in) ::  ictxt
      integer(ipk_), intent(in) ::  err_act
    end subroutine par_error_handler
  end interface

  interface error
    module subroutine serror()
    end subroutine serror
    module subroutine perror(ictxt,abrt)
      integer(mpik_), intent(in) ::  ictxt
      logical, intent(in), optional  :: abrt
    end subroutine perror
  end interface


  interface error_print_stack
    module subroutine par_error_print_stack(ictxt)
      integer(mpik_), intent(in) ::  ictxt
    end subroutine par_error_print_stack
    module subroutine ser_error_print_stack()
    end subroutine ser_error_print_stack
  end interface

  interface errcomm
    module subroutine errcomm(ictxt, err)
      integer(mpik_), intent(in)   :: ictxt
      integer(ipk_), intent(inout):: err
    end subroutine errcomm
  end interface errcomm


  private

  type errstack_node

    integer(ipk_) ::   err_code=0
    character(len=20)        ::   routine=''
    integer(ipk_),dimension(5)     ::   i_err_data=0
    character(len=40)        ::   a_err_data=''
    type(errstack_node), pointer :: next

  end type errstack_node


  type errstack
    type(errstack_node), pointer :: top => null()
    integer(ipk_) :: n_elems=0
  end type errstack


  type(errstack), save  :: error_stack
  integer(ipk_), save   :: error_status    = no_err_
  integer(ipk_), save   :: verbosity_level = 1
  integer(ipk_), save   :: err_action      = act_abort_
  integer(ipk_), save   :: debug_level     = 0, debug_unit, serial_debug_level=0

contains
end module error_mod

submodule (error_mod) error_impl_mod
  use const_mod
contains
  ! checks whether an error has occurred on one of the processes in the execution pool
  subroutine errcomm(ictxt, err)
    integer(mpik_), intent(in)   :: ictxt
    integer(ipk_), intent(inout):: err


  end subroutine errcomm

  subroutine ser_error_handler(err_act)
    implicit none
    integer(ipk_), intent(inout) ::  err_act

    if (err_act /= act_ret_)     &
         &  call error()
    if (err_act == act_abort_) stop

    return
  end subroutine ser_error_handler

  subroutine par_error_handler(ictxt,err_act)
    implicit none
    integer(mpik_), intent(in) ::  ictxt
    integer(ipk_), intent(in) ::  err_act

    if (err_act == act_print_)     &
         &  call error(ictxt, abrt=.false.)
    if (err_act == act_abort_)      &
         &  call error(ictxt, abrt=.true.)

    return

  end subroutine par_error_handler

  subroutine par_error_print_stack(ictxt)
    integer(mpik_), intent(in) ::  ictxt

    call error(ictxt, abrt=.false.)

  end subroutine par_error_print_stack

  subroutine ser_error_print_stack()

    call error()
  end subroutine ser_error_print_stack

  subroutine serror()

    implicit none

  end subroutine serror

  subroutine perror(ictxt,abrt)
    use const_mod
    implicit none
    integer(mpik_), intent(in) :: ictxt
    logical, intent(in), optional  :: abrt

  end subroutine perror

end submodule error_impl_mod

program testlk
  use error_mod
  implicit none

  call error()

  stop
end program testlk
