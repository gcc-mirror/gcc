! { dg-do compile }
! { dg-options "-ffixed-form -fdec" }
!
! Ensure -fd-lines-as-comments is enabled by default with -fdec.
!
d This is a comment.
D This line, too.
      end
