! { dg-do compile }

program p
  interface
    subroutine x
    end subroutine x
  end interface
contains
  subroutine foo
    !$omp declare variant(x) match(construct={do})
  end
  subroutine bar
    !$omp declare variant(y) match(construct={do}) ! { dg-error "Cannot find symbol 'y'" }
  end
end
