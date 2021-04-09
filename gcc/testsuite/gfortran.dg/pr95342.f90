! { dg-do compile }
! PR fortran/95342 - ICE in gfc_match_subroutine, at fortran/decl.c:7913

module m1
  interface
     module subroutine s()
     end
     subroutine s() bind(c) ! { dg-error "EXTERNAL attribute conflicts" }
     end                    ! { dg-error "END INTERFACE" }
  end interface
end

module m2
  interface
     module function f()
     end
     function f() bind(c)
     end                    ! { dg-error "Duplicate EXTERNAL attribute" }
  end interface
end
