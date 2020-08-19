! { dg-do compile }

program p
   interface s
      subroutine g(x, *)
      end
      subroutine h(y, *)
      end
   end interface
end

! { dg-warning "Obsolescent feature: Alternate-return argument" " " { target *-*-* } 5 }
! { dg-warning "Obsolescent feature: Alternate-return argument" " " { target *-*-* } 7 }
! { dg-error ".1." " " { target *-*-* } 5 }
! { dg-error "Ambiguous interfaces in generic interface" " " { target *-*-* } 7 }

