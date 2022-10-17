! { dg-do compile }
! { dg-options "-Wline-truncation" }
print *, 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa'
end 
! { dg-error "Line truncated" " " { target *-*-* } 3 }
! { dg-error "Unterminated character constant" " " { target *-*-* } 3 }
! { dg-prune-output "some warnings being treated as errors" }
