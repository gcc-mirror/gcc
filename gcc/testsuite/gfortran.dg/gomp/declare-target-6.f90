subroutine sub  ! { dg-error "SUBROUTINE attribute conflicts with OMP DECLARE TARGET LINK attribute in 'sub'" }
  !$omp declare target link(sub)
end subroutine sub

subroutine sub2  ! { dg-error "SUBROUTINE attribute conflicts with OMP DECLARE TARGET LOCAL attribute in 'sub2'" }
  !$omp declare target local(sub2)
end subroutine sub2

integer function func()  ! { dg-error "PROCEDURE attribute conflicts with OMP DECLARE TARGET LINK attribute in 'func'" }
  !$omp declare target link(func)
end

integer function func2()  ! { dg-error "PROCEDURE attribute conflicts with OMP DECLARE TARGET LOCAL attribute in 'func2'" }
  !$omp declare target local(func2)
end
