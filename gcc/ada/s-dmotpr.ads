------------------------------------------------------------------------------
--                                                                          --
--                         GNAT RUN-TIME COMPONENTS                         --
--                                                                          --
--         S Y S T E M . D I M . M K S . O T H E R _ P R E F I X E S        --
--                                                                          --
--                                  S p e c                                 --
--                                                                          --
--           Copyright (C) 2011-2012, Free Software Foundation, Inc.        --
--                                                                          --
-- GNAT is free software;  you can  redistribute it  and/or modify it under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  GNAT is distributed in the hope that it will be useful, but WITH- --
-- OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY --
-- or FITNESS FOR A PARTICULAR PURPOSE.                                     --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
-- GNAT was originally developed  by the GNAT team at  New York University. --
-- Extensive contributions were provided by Ada Core Technologies Inc.      --
--                                                                          --
------------------------------------------------------------------------------

--  Package that defines some other prefixes for the MKS base unit system.

--  These prefixes have been defined in a child package in order to avoid too
--  many constant declarations in System.Dim_Mks.

package System.Dim.Mks.Other_Prefixes is

   --  SI prefixes for Meter

   pragma Warnings (Off);
   --  Turn off the all the dimension warnings

   ym  : constant Length := 1.0E-24;  -- yocto
   zm  : constant Length := 1.0E-21;  -- zepto
   am  : constant Length := 1.0E-18;  -- atto
   fm  : constant Length := 1.0E-15;  -- femto
   pm  : constant Length := 1.0E-12;  -- pico
   nm  : constant Length := 1.0E-09;  -- nano
   Gm  : constant Length := 1.0E+09;  -- giga
   Tm  : constant Length := 1.0E+12;  -- tera
   Pem : constant Length := 1.0E+15;  -- peta
   Em  : constant Length := 1.0E+18;  -- exa
   Zem : constant Length := 1.0E+21;  -- zetta
   Yom : constant Length := 1.0E+24;  -- yotta

   --  SI prefixes for Kilogram

   yg  : constant Mass := 1.0E-27;  -- yocto
   zg  : constant Mass := 1.0E-24;  -- zepto
   ag  : constant Mass := 1.0E-21;  -- atto
   fg  : constant Mass := 1.0E-18;  -- femto
   pg  : constant Mass := 1.0E-15;  -- pico
   ng  : constant Mass := 1.0E-12;  -- nano
   Gg  : constant Mass := 1.0E+06;  -- giga
   Tg  : constant Mass := 1.0E+09;  -- tera
   Peg : constant Mass := 1.0E+13;  -- peta
   Eg  : constant Mass := 1.0E+15;  -- exa
   Zeg : constant Mass := 1.0E+18;  -- zetta
   Yog : constant Mass := 1.0E+21;  -- yotta

   --  SI prefixes for Second

   ys  : constant Time := 1.0E-24;  -- yocto
   zs  : constant Time := 1.0E-21;  -- zepto
   as  : constant Time := 1.0E-18;  -- atto
   fs  : constant Time := 1.0E-15;  -- femto
   ps  : constant Time := 1.0E-12;  -- pico
   ns  : constant Time := 1.0E-09;  -- nano
   Gs  : constant Time := 1.0E+09;  -- giga
   Ts  : constant Time := 1.0E+12;  -- tera
   Pes : constant Time := 1.0E+15;  -- peta
   Es  : constant Time := 1.0E+18;  -- exa
   Zes : constant Time := 1.0E+21;  -- zetta
   Yos : constant Time := 1.0E+24;  -- yotta

   --  SI prefixes for Ampere

   yA  : constant Electric_Current := 1.0E-24;  -- yocto
   zA  : constant Electric_Current := 1.0E-21;  -- zepto
   aA  : constant Electric_Current := 1.0E-18;  -- atto
   fA  : constant Electric_Current := 1.0E-15;  -- femto
   nA  : constant Electric_Current := 1.0E-09;  -- nano
   uA  : constant Electric_Current := 1.0E-06;  -- micro (u)
   GA  : constant Electric_Current := 1.0E+09;  -- giga
   TA  : constant Electric_Current := 1.0E+12;  -- tera
   PeA : constant Electric_Current := 1.0E+15;  -- peta
   EA  : constant Electric_Current := 1.0E+18;  -- exa
   ZeA : constant Electric_Current := 1.0E+21;  -- zetta
   YoA : constant Electric_Current := 1.0E+24;  -- yotta

   --  SI prefixes for Kelvin

   yK  : constant Thermodynamic_Temperature := 1.0E-24;  -- yocto
   zK  : constant Thermodynamic_Temperature := 1.0E-21;  -- zepto
   aK  : constant Thermodynamic_Temperature := 1.0E-18;  -- atto
   fK  : constant Thermodynamic_Temperature := 1.0E-15;  -- femto
   pK  : constant Thermodynamic_Temperature := 1.0E-12;  -- pico
   nK  : constant Thermodynamic_Temperature := 1.0E-09;  -- nano
   uK  : constant Thermodynamic_Temperature := 1.0E-06;  -- micro (u)
   mK  : constant Thermodynamic_Temperature := 1.0E-03;  -- milli
   cK  : constant Thermodynamic_Temperature := 1.0E-02;  -- centi
   dK  : constant Thermodynamic_Temperature := 1.0E-01;  -- deci
   daK : constant Thermodynamic_Temperature := 1.0E+01;  -- deka
   hK  : constant Thermodynamic_Temperature := 1.0E+02;  -- hecto
   kK  : constant Thermodynamic_Temperature := 1.0E+03;  -- kilo
   MeK : constant Thermodynamic_Temperature := 1.0E+06;  -- mega
   GK  : constant Thermodynamic_Temperature := 1.0E+09;  -- giga
   TK  : constant Thermodynamic_Temperature := 1.0E+12;  -- tera
   PeK : constant Thermodynamic_Temperature := 1.0E+15;  -- peta
   EK  : constant Thermodynamic_Temperature := 1.0E+18;  -- exa
   ZeK : constant Thermodynamic_Temperature := 1.0E+21;  -- zetta
   YoK : constant Thermodynamic_Temperature := 1.0E+24;  -- yotta

   --  SI prefixes for Mole

   ymol  : constant Amount_Of_Substance := 1.0E-24;  -- yocto
   zmol  : constant Amount_Of_Substance := 1.0E-21;  -- zepto
   amol  : constant Amount_Of_Substance := 1.0E-18;  -- atto
   fmol  : constant Amount_Of_Substance := 1.0E-15;  -- femto
   pmol  : constant Amount_Of_Substance := 1.0E-12;  -- pico
   nmol  : constant Amount_Of_Substance := 1.0E-09;  -- nano
   umol  : constant Amount_Of_Substance := 1.0E-06;  -- micro (u)
   mmol  : constant Amount_Of_Substance := 1.0E-03;  -- milli
   cmol  : constant Amount_Of_Substance := 1.0E-02;  -- centi
   dmol  : constant Amount_Of_Substance := 1.0E-01;  -- deci
   damol : constant Amount_Of_Substance := 1.0E+01;  -- deka
   hmol  : constant Amount_Of_Substance := 1.0E+02;  -- hecto
   kmol  : constant Amount_Of_Substance := 1.0E+03;  -- kilo
   Memol : constant Amount_Of_Substance := 1.0E+06;  -- mega
   Gmol  : constant Amount_Of_Substance := 1.0E+09;  -- giga
   Tmol  : constant Amount_Of_Substance := 1.0E+12;  -- tera
   Pemol : constant Amount_Of_Substance := 1.0E+15;  -- peta
   Emol  : constant Amount_Of_Substance := 1.0E+18;  -- exa
   Zemol : constant Amount_Of_Substance := 1.0E+21;  -- zetta
   Yomol : constant Amount_Of_Substance := 1.0E+24;  -- yotta

   --  SI prefixes for Candela

   ycd  : constant Luminous_Intensity := 1.0E-24;  -- yocto
   zcd  : constant Luminous_Intensity := 1.0E-21;  -- zepto
   acd  : constant Luminous_Intensity := 1.0E-18;  -- atto
   fcd  : constant Luminous_Intensity := 1.0E-15;  -- femto
   pcd  : constant Luminous_Intensity := 1.0E-12;  -- pico
   ncd  : constant Luminous_Intensity := 1.0E-09;  -- nano
   ucd  : constant Luminous_Intensity := 1.0E-06;  -- micro (u)
   mcd  : constant Luminous_Intensity := 1.0E-03;  -- milli
   ccd  : constant Luminous_Intensity := 1.0E-02;  -- centi
   dcd  : constant Luminous_Intensity := 1.0E-01;  -- deci
   dacd : constant Luminous_Intensity := 1.0E+01;  -- deka
   hcd  : constant Luminous_Intensity := 1.0E+02;  -- hecto
   kcd  : constant Luminous_Intensity := 1.0E+03;  -- kilo
   Mecd : constant Luminous_Intensity := 1.0E+06;  -- mega
   Gcd  : constant Luminous_Intensity := 1.0E+09;  -- giga
   Tcd  : constant Luminous_Intensity := 1.0E+12;  -- tera
   Pecd : constant Luminous_Intensity := 1.0E+15;  -- peta
   Ecd  : constant Luminous_Intensity := 1.0E+18;  -- exa
   Zecd : constant Luminous_Intensity := 1.0E+21;  -- zetta
   Yocd : constant Luminous_Intensity := 1.0E+24;  -- yotta

   pragma Warnings (On);
end System.Dim.Mks.Other_Prefixes;
