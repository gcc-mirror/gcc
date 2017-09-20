/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                                 S C O S                                  *
 *                                                                          *
 *                              C Header File                               *
 *                                                                          *
 *           Copyright (C) 2014-2017, Free Software Foundation, Inc.        *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 3,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License *
 * for  more details.  You should have  received  a copy of the GNU General *
 * Public License  distributed with GNAT; see file COPYING3.  If not, go to *
 * http://www.gnu.org/licenses for a complete copy of the license.          *
 *                                                                          *
 * GNAT was originally developed  by the GNAT team at  New York University. *
 * Extensive contributions were provided by Ada Core Technologies Inc.      *
 *                                                                          *
 ****************************************************************************/

/* This is the C file that corresponds to the Ada package spec SCOs.  It was
   created manually from the file scos.ads.  */

#ifdef __cplusplus
extern "C" {
#endif


/* Unit table:  */

typedef Int SCO_Unit_Index;

struct SCO_Unit_Table_Entry
  {
    String_Pointer File_Name;
    Int File_Index;
    Nat Dep_Num;
    Nat From, To;
  };

typedef struct SCO_Unit_Table_Entry *SCO_Unit_Table_Type;

extern SCO_Unit_Table_Type scos__sco_unit_table__table;
#define SCO_Unit_Table scos__sco_unit_table__table

extern Int scos__sco_unit_table__min;
#define SCO_Unit_Table_Min scos__sco_unit_table__min

extern Int scos__sco_unit_table__last_val;
#define SCO_Unit_Table_Last_Val scos__sco_unit_table__last_val


/* SCOs table:  */

struct Source_Location
  {
    Line_Number_Type Line;
    Column_Number_Type Col;
  };

struct SCO_Table_Entry
  {
    struct Source_Location From, To;
    char C1, C2;
    bool Last;
    Source_Ptr Pragma_Sloc;
    Name_Id Pragma_Aspect_Name;
  };

typedef struct SCO_Table_Entry *SCO_Table_Type;

extern SCO_Table_Type scos__sco_table__table;
#define SCO_Table scos__sco_table__table

extern Int scos__sco_table__min;
#define SCO_Table_Min scos__sco_table__min

extern Int scos__sco_table__last_val;
#define SCO_Table_Last_Val scos__sco_table__last_val

#ifdef __cplusplus
}
#endif
