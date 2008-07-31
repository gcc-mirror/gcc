--  { dg-do compile }
--  { dg-options "-gnatc" }

generic
   type T_Item is private;
function genericppc (T : in t_Item; I : integer) return integer;
pragma Precondition (I > 0);
