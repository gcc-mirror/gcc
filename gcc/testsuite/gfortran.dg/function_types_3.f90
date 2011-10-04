! { dg-do compile }
!
! Contributed by Vittorio Zecca <zeccav@gmail.com>
!
! PR 50401: SIGSEGV in resolve_transfer

  interface 
    function f()      ! { dg-error "must be a dummy argument" }
      dimension f(*)
    end function
  end interface
  print *,f()
end

! PR 50403: SIGSEGV in gfc_use_derived

type(f) function f()  ! { dg-error "conflicts with DERIVED attribute|is not accessible" }
  f=110               ! { dg-error "Unclassifiable statement" }
end
