c { dg-do compile }
       integer*1 one
       integer*2 two
       parameter (one=1)
       parameter (two=2)
       select case (I)
       case (one) ! { dg-error "Expression in CASE statement" "" }
       case (two) ! { dg-error "Expression in CASE statement" "" }
       end select
       end
    
