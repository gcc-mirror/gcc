! { dg-do compile }
! PR fortran/102333

function factory() result(payload)
  class(*), allocatable :: payload
contains
  function pointer_callback()
    procedure(factory), pointer :: pointer_callback
  end function pointer_callback
  function allocatable_callback()
    procedure(factory), allocatable :: allocatable_callback ! { dg-error "PROCEDURE attribute conflicts with ALLOCATABLE" }
  end function allocatable_callback
end function factory
