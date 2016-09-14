  ! { dg-do "compile" }
  ! { dg-options "-fdec-structure" }
  !
  ! Test that structures inside a common block do not require the
  ! SEQUENCE attribute, as derived types do.
  !

common var

structure /s/
  integer i
  integer j
  real r
end structure

record /s/ var

end
