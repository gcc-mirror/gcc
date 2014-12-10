! { dg-do compile }
! { dg-options "-Wline-truncation" }
print *, 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'
end 
! { dg-warning "Line truncated" " " { target *-*-* } 3 }
! { dg-error "Unterminated character constant" " " { target *-*-* } 3 }
