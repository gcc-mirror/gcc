! { dg-do compile }
! PR69043 Trying to include a directory causes an infinite loop
      include '.'
      program main
      end program
! { dg-error "is not a regular file" " " { target *-*-* } 3 }
! { dg-prune-output "compilation terminated." }
