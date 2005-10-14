! { dg-do compile }
  integer, dimension (2) :: i
  i = (/Z'abcde', Z'abcde/)	! { dg-error "Illegal character" }
end
