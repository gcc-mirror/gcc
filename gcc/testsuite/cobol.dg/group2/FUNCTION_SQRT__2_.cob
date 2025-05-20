       *> { dg-do run }
       *> { dg-output-file "group2/FUNCTION_SQRT__2_.out" }
        program-id. sqbug.
        procedure division.
        if function sqrt (0) = 0    *>    if4034.2
            display 'ok' else display 'bad'.
        display "sqrt(0) " """" function trim (function exception-status) """"
        set last exception to off
        if function sqrt (-0.1) = 0    *>    if4034.2
            display 'ok' else display 'bad'.
        display "sqrt(-0.1) " """" function trim (function exception-status) """"
        goback.

