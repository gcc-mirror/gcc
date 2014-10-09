! { dg-do compile }
! { dg-options "-O2 -fipa-pta" }
MODULE min_heap
  TYPE heap_t
  END TYPE heap_t
CONTAINS
  ELEMENTAL FUNCTION get_left_child(n) RESULT (child)
    INTEGER, INTENT(IN)                      :: n
  END FUNCTION get_left_child
  ELEMENTAL FUNCTION get_value(heap, n) RESULT (value)
    TYPE(heap_t), INTENT(IN)                 :: heap
    INTEGER, INTENT(IN)                      :: n
  END FUNCTION get_value
END MODULE min_heap

