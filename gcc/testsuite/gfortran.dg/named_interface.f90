! { dg-do compile }
! PR 20363
module snafu
  interface foo
    subroutine really_snafu (foo)        
      integer, intent (inout)  :: foo
    end subroutine really_snafu
  end interface foo
end module snafu
