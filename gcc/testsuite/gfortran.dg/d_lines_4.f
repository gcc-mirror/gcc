! { dg-do compile }
c verify that debug lines are rejected if none of -fd-lines-as-* are given.
d ! { dg-error "Non-numeric character" }
