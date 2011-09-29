! { dg-do compile }
!
! PR 50547: dummy procedure argument of PURE shall be PURE
!
! Contributed by Vittorio Zecca <zeccav@gmail.com>

pure function f(proc)
  interface
    function proc()  ! { dg-error "must also be PURE" }
    end
  end interface
end
