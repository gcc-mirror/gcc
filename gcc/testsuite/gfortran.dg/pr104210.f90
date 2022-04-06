! { dg-do compile }
! { dg-options "-fcoarray=single" }
! PR fortran/104210
! Contributed by G.Steinmetz

function f()      ! { dg-error "shall not be a coarray" }
  integer :: f[*]
end
program p
  interface
     function f() ! { dg-error "shall not be a coarray" }
       integer :: f[*]
     end
  end interface
end
