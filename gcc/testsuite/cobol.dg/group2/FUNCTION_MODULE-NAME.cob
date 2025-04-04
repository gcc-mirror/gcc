       *> { dg-do run }
       *> { dg-output-file "group2/FUNCTION_MODULE-NAME.out" }

        identification          division.
        program-id.             level-1.
        data                    division.
        working-storage         section.
        procedure               division.
        display "From level-1:"
        perform                 reportt.
        call                    "level-2"
        goback.
        reportt.
        display "   "  "top-level:  "  """" function module-name(top-level)  """"
        display "   "  "current:    "  """" function module-name(current)    """"
        display "   "  "activating: "  """" function module-name(activating) """"
        display "   "  "nested:     "  """" function module-name(nested)     """"
        display "   "  "stack:      "  """" function module-name(stack)      """"
        continue.
        end program             level-1.

        identification          division.
        program-id.             level-2.
        data                    division.
        working-storage         section.
        procedure               division.
        display "From level-2:"
        perform                 reportt.
        call "level-3"
        goback.
        reportt.
        display "   "  "top-level:  "  """" function module-name(top-level)  """"
        display "   "  "current:    "  """" function module-name(current)    """"
        display "   "  "activating: "  """" function module-name(activating) """"
        display "   "  "nested:     "  """" function module-name(nested)     """"
        display "   "  "stack:      "  """" function module-name(stack)      """"
        continue.
        end program             level-2.

        identification          division.
        program-id.             level-3.
        data                    division.
        working-storage         section.
        procedure               division.
        display "From level-3:"
        perform                 reportt.
        call "level-3a"
        goback.
        reportt.
        display "   "  "top-level:  "  """" function module-name(top-level)  """"
        display "   "  "current:    "  """" function module-name(current)    """"
        display "   "  "activating: "  """" function module-name(activating) """"
        display "   "  "nested:     "  """" function module-name(nested)     """"
        display "   "  "stack:      "  """" function module-name(stack)      """"
        continue.

        identification          division.
        program-id.             level-3a.
        data                    division.
        working-storage         section.
        procedure               division.
        display "From level-3a:"
        perform                 reportt.
        call "level-3b"
        goback.
        reportt.
        display "   "  "top-level:  "  """" function module-name(top-level)  """"
        display "   "  "current:    "  """" function module-name(current)    """"
        display "   "  "activating: "  """" function module-name(activating) """"
        display "   "  "nested:     "  """" function module-name(nested)     """"
        display "   "  "stack:      "  """" function module-name(stack)      """"
        continue.
        
        identification          division.
        program-id.             level-3b.
        data                    division.
        working-storage         section.
        procedure               division.
        display "From level-3b:"
        perform                 reportt.
        call "level-3c"
        goback.
        reportt.
        display "   "  "top-level:  "  """" function module-name(top-level)  """"
        display "   "  "current:    "  """" function module-name(current)    """"
        display "   "  "activating: "  """" function module-name(activating) """"
        display "   "  "nested:     "  """" function module-name(nested)     """"
        display "   "  "stack:      "  """" function module-name(stack)      """"
        continue.

        identification          division.
        program-id.             level-3c.
        data                    division.
        working-storage         section.
        procedure               division.
        display "From level-3c:"
        perform                 reportt.
        goback.
        reportt.
        display "   "  "top-level:  "  """" function module-name(top-level)  """"
        display "   "  "current:    "  """" function module-name(current)    """"
        display "   "  "activating: "  """" function module-name(activating) """"
        display "   "  "nested:     "  """" function module-name(nested)     """"
        display "   "  "stack:      "  """" function module-name(stack)      """"
        continue.
        end program             level-3c.
        end program             level-3b.
        end program             level-3a.
        end program             level-3.

