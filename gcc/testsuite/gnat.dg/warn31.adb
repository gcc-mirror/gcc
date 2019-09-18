--  { dg-do compile }
--  { dg-options "-gnatw.x -gnatd.a" }
package body Warn31 is
    procedure Dummy is null;
end Warn31;
