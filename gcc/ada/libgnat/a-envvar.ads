------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--              A D A . E N V I R O N M E N T _ V A R I A B L E S           --
--                                                                          --
--                                 S p e c                                  --
--                                                                          --
-- This specification is derived from the Ada Reference Manual for use with --
-- GNAT.  In accordance with the copyright of that document, you can freely --
-- copy and modify this specification,  provided that if you redistribute a --
-- modified version,  any changes that you have made are clearly indicated. --
--                                                                          --
------------------------------------------------------------------------------

--  The implementation of this package is as defined in the Ada 2012 RM, but
--  it is available in Ada 95 and Ada 2005 modes as well.

package Ada.Environment_Variables is
   pragma Preelaborate (Environment_Variables);

   function Value (Name : String) return String;
   --  If the external execution environment supports environment variables,
   --  then Value returns the value of the environment variable with the given
   --  name. If no environment variable with the given name exists, then
   --  Constraint_Error is propagated. If the execution environment does not
   --  support environment variables, then Program_Error is propagated.

   function Value (Name : String; Default : String) return String;
   --  If the external execution environment supports environment variables and
   --  an environment variable with the given name currently exists, then Value
   --  returns its value; otherwise, it returns Default.

   function Exists (Name : String) return Boolean;
   --  If the external execution environment supports environment variables and
   --  an environment variable with the given name currently exists, then
   --  Exists returns True; otherwise it returns False.

   procedure Set (Name : String; Value : String);
   --  If the external execution environment supports environment variables,
   --  then Set first clears any existing environment variable with the given
   --  name, and then defines a single new environment variable with the given
   --  name and value. Otherwise Program_Error is propagated.
   --
   --  If implementation-defined circumstances prohibit the definition of an
   --  environment variable with the given name and value, then exception
   --  Constraint_Error is propagated.
   --
   --  It is implementation defined whether there exist values for which the
   --  call Set (Name, Value) has the same effect as Clear (Name).

   procedure Clear (Name : String);
   --  If the external execution environment supports environment variables,
   --  then Clear deletes all existing environment variables with the given
   --  name. Otherwise Program_Error is propagated.

   procedure Clear;
   --  If the external execution environment supports environment variables,
   --  then Clear deletes all existing environment variables. Otherwise
   --  Program_Error is propagated.

   procedure Iterate
     (Process : not null access procedure (Name, Value : String));
   --  If the external execution environment supports environment variables,
   --  then Iterate calls the subprogram designated by Process for each
   --  existing environment variable, passing the name and value of that
   --  environment variable. Otherwise Program_Error is propagated.

end Ada.Environment_Variables;
