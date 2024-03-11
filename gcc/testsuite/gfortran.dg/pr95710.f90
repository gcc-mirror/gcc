! { dg-do compile }
! PR fortran/95710 - ICE on duplicate declaration of class variable
! Contributed by G.Steinmetz

module m
  interface
     module function s()
     end
  end interface
end
submodule(m) m2
contains
  module function s()
    class(*), allocatable :: x
    class(*), allocatable :: x ! { dg-error "Unclassifiable statement" }
  end
end
