------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUNTIME COMPONENTS                          --
--                                                                          --
--                 A D A . I N T E R R U P T S . N A M E S                  --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
-- This specification is adapted from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

--  The standard implementation of this spec contains only dummy interrupt
--  names. These dummy entries permit checking out code for correctness of
--  semantics, even if interrupts are not supported.

--  For specific implementations that fully support interrupts, this package
--  spec is replaced by an implementation dependent version that defines the
--  interrupts available on the system.

package Ada.Interrupts.Names is

   DUMMY_INTERRUPT_1 : constant Interrupt_ID := 1;
   DUMMY_INTERRUPT_2 : constant Interrupt_ID := 2;

end Ada.Interrupts.Names;
