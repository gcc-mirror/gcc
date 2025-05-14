       *> { dg-do run }
       *> { dg-output-file "group2/Refmod_sources_are_figurative_constants.out" }

        id division.
        program-id. prog.
        data division.
        working-storage section.
        01 varx pic x(8) VALUE '""""""""'.
        01 varp redefines varx pointer.
        procedure division.
        move "12345678" to varx
        display  """" varx """"
        move "999" to varx(4:3)
        display  """" varx """"
        move LOW-VALUE to varx(4:3).
        display  """" varx """"
        move ZERO to varx(4:3).
        display  """" varx """"
        move SPACE to varx(4:3).
        display  """" varx """"
        move QUOTE to varx(4:3).
        display  """" varx """"
        move HIGH-VALUE to varx(4:3).
        display  varp
        initialize varx all to value
        display  """" varx """"
        .
        end program prog.

