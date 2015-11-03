! { dg-do compile }
!
! Contributed by Vittorio Zecca <zeccav@gmail.com>
!
! PR 50401: SIGSEGV in resolve_transfer

  interface 
    function f()      ! { dg-error "must be a dummy argument|Interface mismatch in global procedure" }
      dimension f(*)
    end function
  end interface
  print *,f()
end

! PR 50403: SIGSEGV in gfc_use_derived

type(f) function f()  ! { dg-error "Type name 'f' at .1. conflicts with previously declared entity|The type for function 'f' at .1. is not accessible" }
  f=110               ! { dg-error "Type inaccessible in variable definition context" }
end
