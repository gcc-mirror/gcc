/****************************************************************************
 *                                                                          *
 *                         GNAT COMPILER COMPONENTS                         *
 *                                                                          *
 *                                A T R E E                                 *
 *                                                                          *
 *                              C Header File                               *
 *                                                                          *
 *          Copyright (C) 1992-2003, Free Software Foundation, Inc.         *
 *                                                                          *
 * GNAT is free software;  you can  redistribute it  and/or modify it under *
 * terms of the  GNU General Public License as published  by the Free Soft- *
 * ware  Foundation;  either version 2,  or (at your option) any later ver- *
 * sion.  GNAT is distributed in the hope that it will be useful, but WITH- *
 * OUT ANY WARRANTY;  without even the  implied warranty of MERCHANTABILITY *
 * or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License *
 * for  more details.  You should have  received  a copy of the GNU General *
 * Public License  distributed with GNAT;  see file COPYING.  If not, write *
 * to  the Free Software Foundation,  59 Temple Place - Suite 330,  Boston, *
 * MA 02111-1307, USA.                                                      *
 *                                                                          *
 * GNAT was originally developed  by the GNAT team at  New York University. *
 * Extensive contributions were provided by Ada Core Technologies Inc.      *
 *                                                                          *
 ****************************************************************************/

/* This is the C header corresponding to the Ada package specification for
   Atree. It also contains the implementations of inlined functions from the
   package body for Tree.  It was generated manually from atree.ads and
   atree.adb and must be kept synchronized with changes in these files.

   Note that only routines for reading the tree are included, since the tree
   transformer is not supposed to modify the tree in any way. */

/* Structure used for the first part of the node in the case where we have
   an Nkind.  */

struct NFK
{
  Boolean      is_extension      :  1;
  Boolean      pflag1            :  1;
  Boolean      pflag2            :  1;
  Boolean      in_list           :  1;
  Boolean      rewrite_sub       :  1;
  Boolean      rewrite_ins       :  1;
  Boolean      analyzed          :  1;
  Boolean      c_f_s		 :  1;

  Boolean      error_posted  :  1;
  Boolean      flag4  :  1;
  Boolean      flag5  :  1;
  Boolean      flag6  :  1;
  Boolean      flag7  :  1;
  Boolean      flag8  :  1;
  Boolean      flag9  :  1;
  Boolean      flag10 :  1;

  Boolean      flag11 :  1;
  Boolean      flag12 :  1;
  Boolean      flag13 :  1;
  Boolean      flag14 :  1;
  Boolean      flag15 :  1;
  Boolean      flag16 :  1;
  Boolean      flag17 :  1;
  Boolean      flag18 :  1;

  unsigned char kind;
};

/* Structure for the first part of a node when Nkind is not present by
   extra flag bits are.  */

struct NFNK
{
  Boolean      is_extension      :  1;
  Boolean      pflag1            :  1;
  Boolean      pflag2            :  1;
  Boolean      in_list           :  1;
  Boolean      rewrite_sub       :  1;
  Boolean      rewrite_ins       :  1;
  Boolean      analyzed          :  1;
  Boolean      c_f_s		 :  1;

  Boolean      error_posted  :  1;
  Boolean      flag4  :  1;
  Boolean      flag5  :  1;
  Boolean      flag6  :  1;
  Boolean      flag7  :  1;
  Boolean      flag8  :  1;
  Boolean      flag9  :  1;
  Boolean      flag10 :  1;

  Boolean      flag11 :  1;
  Boolean      flag12 :  1;
  Boolean      flag13 :  1;
  Boolean      flag14 :  1;
  Boolean      flag15 :  1;
  Boolean      flag16 :  1;
  Boolean      flag17 :  1;
  Boolean      flag18 :  1;

  Boolean      flag65 :  1;
  Boolean      flag66 :  1;
  Boolean      flag67 :  1;
  Boolean      flag68 :  1;
  Boolean      flag69 :  1;
  Boolean      flag70 :  1;
  Boolean      flag71 :  1;
  Boolean      flag72 :  1;
};

/* Structure used for extra flags in third component overlaying Field12 */
struct Flag_Word
{
  Boolean      flag73	    :  1;
  Boolean      flag74	    :  1;
  Boolean      flag75	    :  1;
  Boolean      flag76	    :  1;
  Boolean      flag77	    :  1;
  Boolean      flag78	    :  1;
  Boolean      flag79	    :  1;
  Boolean      flag80	    :  1;
  Boolean      flag81	    :  1;
  Boolean      flag82	    :  1;
  Boolean      flag83	    :  1;
  Boolean      flag84	    :  1;
  Boolean      flag85	    :  1;
  Boolean      flag86	    :  1;
  Boolean      flag87	    :  1;
  Boolean      flag88	    :  1;
  Boolean      flag89	    :  1;
  Boolean      flag90	    :  1;
  Boolean      flag91	    :  1;
  Boolean      flag92	    :  1;
  Boolean      flag93	    :  1;
  Boolean      flag94	    :  1;
  Boolean      flag95	    :  1;
  Boolean      flag96	    :  1;
  Short        convention   :  8;
};

/* Structure used for extra flags in fourth component overlaying Field12 */
struct Flag_Word2
{
  Boolean      flag97	    :  1;
  Boolean      flag98	    :  1;
  Boolean      flag99	    :  1;
  Boolean      flag100	    :  1;
  Boolean      flag101	    :  1;
  Boolean      flag102	    :  1;
  Boolean      flag103	    :  1;
  Boolean      flag104	    :  1;
  Boolean      flag105	    :  1;
  Boolean      flag106	    :  1;
  Boolean      flag107	    :  1;
  Boolean      flag108	    :  1;
  Boolean      flag109	    :  1;
  Boolean      flag110	    :  1;
  Boolean      flag111	    :  1;
  Boolean      flag112	    :  1;
  Boolean      flag113	    :  1;
  Boolean      flag114	    :  1;
  Boolean      flag115	    :  1;
  Boolean      flag116	    :  1;
  Boolean      flag117	    :  1;
  Boolean      flag118	    :  1;
  Boolean      flag119	    :  1;
  Boolean      flag120	    :  1;
  Boolean      flag121	    :  1;
  Boolean      flag122	    :  1;
  Boolean      flag123	    :  1;
  Boolean      flag124	    :  1;
  Boolean      flag125	    :  1;
  Boolean      flag126	    :  1;
  Boolean      flag127	    :  1;
  Boolean      flag128	    :  1;
};

/* Structure used for extra flags in fourth component overlaying Field11 */
struct Flag_Word3
{
  Boolean      flag152	    :  1;
  Boolean      flag153	    :  1;
  Boolean      flag154	    :  1;
  Boolean      flag155	    :  1;
  Boolean      flag156	    :  1;
  Boolean      flag157	    :  1;
  Boolean      flag158	    :  1;
  Boolean      flag159	    :  1;

  Boolean      flag160	    :  1;
  Boolean      flag161	    :  1;
  Boolean      flag162	    :  1;
  Boolean      flag163	    :  1;
  Boolean      flag164	    :  1;
  Boolean      flag165	    :  1;
  Boolean      flag166	    :  1;
  Boolean      flag167	    :  1;

  Boolean      flag168	    :  1;
  Boolean      flag169	    :  1;
  Boolean      flag170	    :  1;
  Boolean      flag171	    :  1;
  Boolean      flag172	    :  1;
  Boolean      flag173	    :  1;
  Boolean      flag174	    :  1;
  Boolean      flag175	    :  1;

  Boolean      flag176	    :  1;
  Boolean      flag177	    :  1;
  Boolean      flag178	    :  1;
  Boolean      flag179	    :  1;
  Boolean      flag180	    :  1;
  Boolean      flag181	    :  1;
  Boolean      flag182	    :  1;
  Boolean      flag183	    :  1;
};

struct Non_Extended
{
  Source_Ptr   sloc;
  Int	       link;
  Int	       field1;
  Int	       field2;
  Int	       field3;
  Int	       field4;
  Int	       field5;
};

/* The Following structure corresponds to variant with is_extension = True.  */
struct Extended
{
  Int	       field6;
  Int	       field7;
  Int	       field8;
  Int	       field9;
  Int	       field10;
  union
    {
      Int      field11;
      struct Flag_Word3 fw3;
    } X;

  union
    {
      Int      field12;
      struct Flag_Word fw;
      struct Flag_Word2 fw2;
    } U;
};

/* A tree node itself.  */

struct Node
{
  union kind
    {
      struct NFK K;
      struct NFNK NK;
    } U;

  union variant
    {
      struct Non_Extended NX;
      struct Extended EX;
    } V;
};

/* The actual tree is an array of nodes. The pointer to this array is passed
   as a parameter to the tree transformer procedure and stored in the global
   variable Nodes_Ptr after adjusting it by subtracting Node_First_Entry, so
   that Node_Id values can be used as subscripts.  */
extern struct Node *Nodes_Ptr;


#define Parent atree__parent
extern Node_Id Parent (Node_Id);

/* Overloaded Functions:

   These functions are overloaded in the original Ada source, but there is
   only one corresponding C function, which works as described below.	*/

/* Type used for union of Node_Id, List_Id, Elist_Id. */
typedef Int Tree_Id;

/* These two functions can only be used for Node_Id and List_Id values and
   they work in the C version because Empty = No_List = 0.  */

static Boolean No	(Tree_Id);
static Boolean Present	(Tree_Id);

INLINE Boolean
No (Tree_Id N)
{
  return N == Empty;
}

INLINE Boolean
Present (Tree_Id N)
{
  return N != Empty;
}

extern Node_Id Parent		(Tree_Id);

#define Current_Error_Node atree__current_error_node
extern Node_Id Current_Error_Node;

/* Node Access Functions:  */

#define Nkind(N)        ((Node_Kind) (Nodes_Ptr[(N) - First_Node_Id].U.K.kind))
#define Ekind(N)        ((Entity_Kind) (Nodes_Ptr[N + 1].U.K.kind))
#define Sloc(N)         (Nodes_Ptr[(N) - First_Node_Id].V.NX.sloc)
#define Paren_Count(N)	(Nodes_Ptr[(N) - First_Node_Id].U.K.pflag1	\
			 + 2 * Nodes_Ptr[(N) - First_Node_Id].U.K.pflag2)

#define Field1(N)     (Nodes_Ptr[(N) - First_Node_Id].V.NX.field1)
#define Field2(N)     (Nodes_Ptr[(N) - First_Node_Id].V.NX.field2)
#define Field3(N)     (Nodes_Ptr[(N) - First_Node_Id].V.NX.field3)
#define Field4(N)     (Nodes_Ptr[(N) - First_Node_Id].V.NX.field4)
#define Field5(N)     (Nodes_Ptr[(N) - First_Node_Id].V.NX.field5)
#define Field6(N)     (Nodes_Ptr[(N) - First_Node_Id + 1].V.EX.field6)
#define Field7(N)     (Nodes_Ptr[(N) - First_Node_Id + 1].V.EX.field7)
#define Field8(N)     (Nodes_Ptr[(N) - First_Node_Id + 1].V.EX.field8)
#define Field9(N)     (Nodes_Ptr[(N) - First_Node_Id + 1].V.EX.field9)
#define Field10(N)    (Nodes_Ptr[(N) - First_Node_Id + 1].V.EX.field10)
#define Field11(N)    (Nodes_Ptr[(N) - First_Node_Id + 1].V.EX.X.field11)
#define Field12(N)    (Nodes_Ptr[(N) - First_Node_Id + 1].V.EX.U.field12)
#define Field13(N)    (Nodes_Ptr[(N) - First_Node_Id + 2].V.EX.field6)
#define Field14(N)    (Nodes_Ptr[(N) - First_Node_Id + 2].V.EX.field7)
#define Field15(N)    (Nodes_Ptr[(N) - First_Node_Id + 2].V.EX.field8)
#define Field16(N)    (Nodes_Ptr[(N) - First_Node_Id + 2].V.EX.field9)
#define Field17(N)    (Nodes_Ptr[(N) - First_Node_Id + 2].V.EX.field10)
#define Field18(N)    (Nodes_Ptr[(N) - First_Node_Id + 2].V.EX.X.field11)
#define Field19(N)    (Nodes_Ptr[(N) - First_Node_Id + 3].V.EX.field6)
#define Field20(N)    (Nodes_Ptr[(N) - First_Node_Id + 3].V.EX.field7)
#define Field21(N)    (Nodes_Ptr[(N) - First_Node_Id + 3].V.EX.field8)
#define Field22(N)    (Nodes_Ptr[(N) - First_Node_Id + 3].V.EX.field9)
#define Field23(N)    (Nodes_Ptr[(N) - First_Node_Id + 3].V.EX.field10)

#define Node1(N)      Field1  (N)
#define Node2(N)      Field2  (N)
#define Node3(N)      Field3  (N)
#define Node4(N)      Field4  (N)
#define Node5(N)      Field5  (N)
#define Node6(N)      Field6  (N)
#define Node7(N)      Field7  (N)
#define Node8(N)      Field8  (N)
#define Node9(N)      Field9  (N)
#define Node10(N)     Field10 (N)
#define Node11(N)     Field11 (N)
#define Node12(N)     Field12 (N)
#define Node13(N)     Field13 (N)
#define Node14(N)     Field14 (N)
#define Node15(N)     Field15 (N)
#define Node16(N)     Field16 (N)
#define Node17(N)     Field17 (N)
#define Node18(N)     Field18 (N)
#define Node19(N)     Field19 (N)
#define Node20(N)     Field20 (N)
#define Node21(N)     Field21 (N)
#define Node22(N)     Field22 (N)
#define Node23(N)     Field23 (N)

#define List1(N)      Field1  (N)
#define List2(N)      Field2  (N)
#define List3(N)      Field3  (N)
#define List4(N)      Field4  (N)
#define List5(N)      Field5  (N)
#define List10(N)     Field10 (N)
#define List14(N)     Field14 (N)

#define Elist2(N)     Field2  (N)
#define Elist3(N)     Field3  (N)
#define Elist4(N)     Field4  (N)
#define Elist8(N)     Field8  (N)
#define Elist13(N)    Field13 (N)
#define Elist15(N)    Field15 (N)
#define Elist18(N)    Field18 (N)
#define Elist21(N)    Field21 (N)
#define Elist23(N)    Field23 (N)

#define Name1(N)      Field1  (N)
#define Name2(N)      Field2  (N)

#define Char_Code2(N) (Field2 (N) - Char_Code_Bias)

#define Str3(N)       Field3  (N)

#define Uint3(N)      ((Field3  (N) == 0) ? Uint_0 : Field3  (N))
#define Uint4(N)      ((Field4  (N) == 0) ? Uint_0 : Field4  (N))
#define Uint5(N)      ((Field5  (N) == 0) ? Uint_0 : Field5  (N))
#define Uint8(N)      ((Field8  (N) == 0) ? Uint_0 : Field8  (N))
#define Uint9(N)      ((Field9  (N) == 0) ? Uint_0 : Field9  (N))
#define Uint10(N)     ((Field10 (N) == 0) ? Uint_0 : Field10 (N))
#define Uint11(N)     ((Field11 (N) == 0) ? Uint_0 : Field11 (N))
#define Uint12(N)     ((Field12 (N) == 0) ? Uint_0 : Field12 (N))
#define Uint13(N)     ((Field13 (N) == 0) ? Uint_0 : Field13 (N))
#define Uint14(N)     ((Field14 (N) == 0) ? Uint_0 : Field14 (N))
#define Uint15(N)     ((Field15 (N) == 0) ? Uint_0 : Field15 (N))
#define Uint16(N)     ((Field16 (N) == 0) ? Uint_0 : Field16 (N))
#define Uint17(N)     ((Field17 (N) == 0) ? Uint_0 : Field17 (N))
#define Uint22(N)     ((Field22 (N) == 0) ? Uint_0 : Field22 (N))

#define Ureal3(N)     Field3  (N)
#define Ureal18(N)    Field18 (N)
#define Ureal21(N)    Field21 (N)

#define Analyzed(N)          (Nodes_Ptr[(N) - First_Node_Id].U.K.analyzed)
#define Comes_From_Source(N) (Nodes_Ptr[(N) - First_Node_Id].U.K.c_f_s)
#define Error_Posted(N)      (Nodes_Ptr[(N) - First_Node_Id].U.K.error_posted)
#define Convention(N) \
    (Nodes_Ptr[(N) - First_Node_Id + 2].V.EX.U.fw.convention)

#define Flag4(N)      (Nodes_Ptr[(N) - First_Node_Id].U.K.flag4)
#define Flag5(N)      (Nodes_Ptr[(N) - First_Node_Id].U.K.flag5)
#define Flag6(N)      (Nodes_Ptr[(N) - First_Node_Id].U.K.flag6)
#define Flag7(N)      (Nodes_Ptr[(N) - First_Node_Id].U.K.flag7)
#define Flag8(N)      (Nodes_Ptr[(N) - First_Node_Id].U.K.flag8)
#define Flag9(N)      (Nodes_Ptr[(N) - First_Node_Id].U.K.flag9)
#define Flag10(N)     (Nodes_Ptr[(N) - First_Node_Id].U.K.flag10)
#define Flag11(N)     (Nodes_Ptr[(N) - First_Node_Id].U.K.flag11)
#define Flag12(N)     (Nodes_Ptr[(N) - First_Node_Id].U.K.flag12)
#define Flag13(N)     (Nodes_Ptr[(N) - First_Node_Id].U.K.flag13)
#define Flag14(N)     (Nodes_Ptr[(N) - First_Node_Id].U.K.flag14)
#define Flag15(N)     (Nodes_Ptr[(N) - First_Node_Id].U.K.flag15)
#define Flag16(N)     (Nodes_Ptr[(N) - First_Node_Id].U.K.flag16)
#define Flag17(N)     (Nodes_Ptr[(N) - First_Node_Id].U.K.flag17)
#define Flag18(N)     (Nodes_Ptr[(N) - First_Node_Id].U.K.flag18)

#define Flag19(N)     (Nodes_Ptr[(N) - First_Node_Id + 1].U.K.in_list)
#define Flag20(N)     (Nodes_Ptr[(N) - First_Node_Id + 1].U.K.rewrite_sub)
#define Flag21(N)     (Nodes_Ptr[(N) - First_Node_Id + 1].U.K.rewrite_ins)
#define Flag22(N)     (Nodes_Ptr[(N) - First_Node_Id + 1].U.K.analyzed)
#define Flag23(N)     (Nodes_Ptr[(N) - First_Node_Id + 1].U.K.c_f_s)
#define Flag24(N)     (Nodes_Ptr[(N) - First_Node_Id + 1].U.K.error_posted)
#define Flag25(N)     (Nodes_Ptr[(N) - First_Node_Id + 1].U.K.flag4)
#define Flag26(N)     (Nodes_Ptr[(N) - First_Node_Id + 1].U.K.flag5)
#define Flag27(N)     (Nodes_Ptr[(N) - First_Node_Id + 1].U.K.flag6)
#define Flag28(N)     (Nodes_Ptr[(N) - First_Node_Id + 1].U.K.flag7)
#define Flag29(N)     (Nodes_Ptr[(N) - First_Node_Id + 1].U.K.flag8)
#define Flag30(N)     (Nodes_Ptr[(N) - First_Node_Id + 1].U.K.flag9)
#define Flag31(N)     (Nodes_Ptr[(N) - First_Node_Id + 1].U.K.flag10)
#define Flag32(N)     (Nodes_Ptr[(N) - First_Node_Id + 1].U.K.flag11)
#define Flag33(N)     (Nodes_Ptr[(N) - First_Node_Id + 1].U.K.flag12)
#define Flag34(N)     (Nodes_Ptr[(N) - First_Node_Id + 1].U.K.flag13)
#define Flag35(N)     (Nodes_Ptr[(N) - First_Node_Id + 1].U.K.flag14)
#define Flag36(N)     (Nodes_Ptr[(N) - First_Node_Id + 1].U.K.flag15)
#define Flag37(N)     (Nodes_Ptr[(N) - First_Node_Id + 1].U.K.flag16)
#define Flag38(N)     (Nodes_Ptr[(N) - First_Node_Id + 1].U.K.flag17)
#define Flag39(N)     (Nodes_Ptr[(N) - First_Node_Id + 1].U.K.flag18)

#define Flag40(N)     (Nodes_Ptr[(N) - First_Node_Id + 2].U.K.in_list)
#define Flag41(N)     (Nodes_Ptr[(N) - First_Node_Id + 2].U.K.rewrite_sub)
#define Flag42(N)     (Nodes_Ptr[(N) - First_Node_Id + 2].U.K.rewrite_ins)
#define Flag43(N)     (Nodes_Ptr[(N) - First_Node_Id + 2].U.K.analyzed)
#define Flag44(N)     (Nodes_Ptr[(N) - First_Node_Id + 2].U.K.c_f_s)
#define Flag45(N)     (Nodes_Ptr[(N) - First_Node_Id + 2].U.K.error_posted)
#define Flag46(N)     (Nodes_Ptr[(N) - First_Node_Id + 2].U.K.flag4)
#define Flag47(N)     (Nodes_Ptr[(N) - First_Node_Id + 2].U.K.flag5)
#define Flag48(N)     (Nodes_Ptr[(N) - First_Node_Id + 2].U.K.flag6)
#define Flag49(N)     (Nodes_Ptr[(N) - First_Node_Id + 2].U.K.flag7)
#define Flag50(N)     (Nodes_Ptr[(N) - First_Node_Id + 2].U.K.flag8)
#define Flag51(N)     (Nodes_Ptr[(N) - First_Node_Id + 2].U.K.flag9)
#define Flag52(N)     (Nodes_Ptr[(N) - First_Node_Id + 2].U.K.flag10)
#define Flag53(N)     (Nodes_Ptr[(N) - First_Node_Id + 2].U.K.flag11)
#define Flag54(N)     (Nodes_Ptr[(N) - First_Node_Id + 2].U.K.flag12)
#define Flag55(N)     (Nodes_Ptr[(N) - First_Node_Id + 2].U.K.flag13)
#define Flag56(N)     (Nodes_Ptr[(N) - First_Node_Id + 2].U.K.flag14)
#define Flag57(N)     (Nodes_Ptr[(N) - First_Node_Id + 2].U.K.flag15)
#define Flag58(N)     (Nodes_Ptr[(N) - First_Node_Id + 2].U.K.flag16)
#define Flag59(N)     (Nodes_Ptr[(N) - First_Node_Id + 2].U.K.flag17)
#define Flag60(N)     (Nodes_Ptr[(N) - First_Node_Id + 2].U.K.flag18)
#define Flag61(N)     (Nodes_Ptr[(N) - First_Node_Id + 1].U.K.pflag1)
#define Flag62(N)     (Nodes_Ptr[(N) - First_Node_Id + 1].U.K.pflag2)
#define Flag63(N)     (Nodes_Ptr[(N) - First_Node_Id + 2].U.K.pflag1)
#define Flag64(N)     (Nodes_Ptr[(N) - First_Node_Id + 2].U.K.pflag2)

#define Flag65(N)     (Nodes_Ptr[(N) - First_Node_Id + 2].U.NK.flag65)
#define Flag66(N)     (Nodes_Ptr[(N) - First_Node_Id + 2].U.NK.flag66)
#define Flag67(N)     (Nodes_Ptr[(N) - First_Node_Id + 2].U.NK.flag67)
#define Flag68(N)     (Nodes_Ptr[(N) - First_Node_Id + 2].U.NK.flag68)
#define Flag69(N)     (Nodes_Ptr[(N) - First_Node_Id + 2].U.NK.flag69)
#define Flag70(N)     (Nodes_Ptr[(N) - First_Node_Id + 2].U.NK.flag70)
#define Flag71(N)     (Nodes_Ptr[(N) - First_Node_Id + 2].U.NK.flag71)
#define Flag72(N)     (Nodes_Ptr[(N) - First_Node_Id + 2].U.NK.flag72)

#define Flag73(N)     (Nodes_Ptr[(N) - First_Node_Id + 2].V.EX.U.fw.flag73)
#define Flag74(N)     (Nodes_Ptr[(N) - First_Node_Id + 2].V.EX.U.fw.flag74)
#define Flag75(N)     (Nodes_Ptr[(N) - First_Node_Id + 2].V.EX.U.fw.flag75)
#define Flag76(N)     (Nodes_Ptr[(N) - First_Node_Id + 2].V.EX.U.fw.flag76)
#define Flag77(N)     (Nodes_Ptr[(N) - First_Node_Id + 2].V.EX.U.fw.flag77)
#define Flag78(N)     (Nodes_Ptr[(N) - First_Node_Id + 2].V.EX.U.fw.flag78)
#define Flag79(N)     (Nodes_Ptr[(N) - First_Node_Id + 2].V.EX.U.fw.flag79)
#define Flag80(N)     (Nodes_Ptr[(N) - First_Node_Id + 2].V.EX.U.fw.flag80)
#define Flag81(N)     (Nodes_Ptr[(N) - First_Node_Id + 2].V.EX.U.fw.flag81)
#define Flag82(N)     (Nodes_Ptr[(N) - First_Node_Id + 2].V.EX.U.fw.flag82)
#define Flag83(N)     (Nodes_Ptr[(N) - First_Node_Id + 2].V.EX.U.fw.flag83)
#define Flag84(N)     (Nodes_Ptr[(N) - First_Node_Id + 2].V.EX.U.fw.flag84)
#define Flag85(N)     (Nodes_Ptr[(N) - First_Node_Id + 2].V.EX.U.fw.flag85)
#define Flag86(N)     (Nodes_Ptr[(N) - First_Node_Id + 2].V.EX.U.fw.flag86)
#define Flag87(N)     (Nodes_Ptr[(N) - First_Node_Id + 2].V.EX.U.fw.flag87)
#define Flag88(N)     (Nodes_Ptr[(N) - First_Node_Id + 2].V.EX.U.fw.flag88)
#define Flag89(N)     (Nodes_Ptr[(N) - First_Node_Id + 2].V.EX.U.fw.flag89)
#define Flag90(N)     (Nodes_Ptr[(N) - First_Node_Id + 2].V.EX.U.fw.flag90)
#define Flag91(N)     (Nodes_Ptr[(N) - First_Node_Id + 2].V.EX.U.fw.flag91)
#define Flag92(N)     (Nodes_Ptr[(N) - First_Node_Id + 2].V.EX.U.fw.flag92)
#define Flag93(N)     (Nodes_Ptr[(N) - First_Node_Id + 2].V.EX.U.fw.flag93)
#define Flag94(N)     (Nodes_Ptr[(N) - First_Node_Id + 2].V.EX.U.fw.flag94)
#define Flag95(N)     (Nodes_Ptr[(N) - First_Node_Id + 2].V.EX.U.fw.flag95)
#define Flag96(N)     (Nodes_Ptr[(N) - First_Node_Id + 2].V.EX.U.fw.flag96)
#define Flag97(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].V.EX.U.fw2.flag97)
#define Flag98(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].V.EX.U.fw2.flag98)
#define Flag99(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].V.EX.U.fw2.flag99)
#define Flag100(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].V.EX.U.fw2.flag100)
#define Flag101(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].V.EX.U.fw2.flag101)
#define Flag102(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].V.EX.U.fw2.flag102)
#define Flag103(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].V.EX.U.fw2.flag103)
#define Flag104(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].V.EX.U.fw2.flag104)
#define Flag105(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].V.EX.U.fw2.flag105)
#define Flag106(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].V.EX.U.fw2.flag106)
#define Flag107(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].V.EX.U.fw2.flag107)
#define Flag108(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].V.EX.U.fw2.flag108)
#define Flag109(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].V.EX.U.fw2.flag109)
#define Flag110(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].V.EX.U.fw2.flag110)
#define Flag111(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].V.EX.U.fw2.flag111)
#define Flag112(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].V.EX.U.fw2.flag112)
#define Flag113(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].V.EX.U.fw2.flag113)
#define Flag114(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].V.EX.U.fw2.flag114)
#define Flag115(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].V.EX.U.fw2.flag115)
#define Flag116(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].V.EX.U.fw2.flag116)
#define Flag117(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].V.EX.U.fw2.flag117)
#define Flag118(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].V.EX.U.fw2.flag118)
#define Flag119(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].V.EX.U.fw2.flag119)
#define Flag120(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].V.EX.U.fw2.flag120)
#define Flag121(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].V.EX.U.fw2.flag121)
#define Flag122(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].V.EX.U.fw2.flag122)
#define Flag123(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].V.EX.U.fw2.flag123)
#define Flag124(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].V.EX.U.fw2.flag124)
#define Flag125(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].V.EX.U.fw2.flag125)
#define Flag126(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].V.EX.U.fw2.flag126)
#define Flag127(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].V.EX.U.fw2.flag127)
#define Flag128(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].V.EX.U.fw2.flag128)

#define Flag129(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].U.K.in_list)
#define Flag130(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].U.K.rewrite_sub)
#define Flag131(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].U.K.rewrite_ins)
#define Flag132(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].U.K.analyzed)
#define Flag133(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].U.K.c_f_s)
#define Flag134(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].U.K.error_posted)
#define Flag135(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].U.K.flag4)
#define Flag136(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].U.K.flag5)
#define Flag137(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].U.K.flag6)
#define Flag138(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].U.K.flag7)
#define Flag139(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].U.K.flag8)
#define Flag140(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].U.K.flag9)
#define Flag141(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].U.K.flag10)
#define Flag142(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].U.K.flag11)
#define Flag143(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].U.K.flag12)
#define Flag144(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].U.K.flag13)
#define Flag145(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].U.K.flag14)
#define Flag146(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].U.K.flag15)
#define Flag147(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].U.K.flag16)
#define Flag148(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].U.K.flag17)
#define Flag149(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].U.K.flag18)
#define Flag150(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].U.K.pflag1)
#define Flag151(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].U.K.pflag2)

#define Flag152(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].V.EX.X.fw3.flag152)
#define Flag153(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].V.EX.X.fw3.flag153)
#define Flag154(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].V.EX.X.fw3.flag154)
#define Flag155(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].V.EX.X.fw3.flag155)
#define Flag156(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].V.EX.X.fw3.flag156)
#define Flag157(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].V.EX.X.fw3.flag157)
#define Flag158(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].V.EX.X.fw3.flag158)
#define Flag159(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].V.EX.X.fw3.flag159)
#define Flag160(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].V.EX.X.fw3.flag160)
#define Flag161(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].V.EX.X.fw3.flag161)
#define Flag162(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].V.EX.X.fw3.flag162)
#define Flag163(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].V.EX.X.fw3.flag163)
#define Flag164(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].V.EX.X.fw3.flag164)
#define Flag165(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].V.EX.X.fw3.flag165)
#define Flag166(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].V.EX.X.fw3.flag166)
#define Flag167(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].V.EX.X.fw3.flag167)
#define Flag168(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].V.EX.X.fw3.flag168)
#define Flag169(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].V.EX.X.fw3.flag169)
#define Flag170(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].V.EX.X.fw3.flag170)
#define Flag171(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].V.EX.X.fw3.flag171)
#define Flag172(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].V.EX.X.fw3.flag172)
#define Flag173(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].V.EX.X.fw3.flag173)
#define Flag174(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].V.EX.X.fw3.flag174)
#define Flag175(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].V.EX.X.fw3.flag175)
#define Flag176(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].V.EX.X.fw3.flag176)
#define Flag177(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].V.EX.X.fw3.flag177)
#define Flag178(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].V.EX.X.fw3.flag178)
#define Flag179(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].V.EX.X.fw3.flag179)
#define Flag180(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].V.EX.X.fw3.flag180)
#define Flag181(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].V.EX.X.fw3.flag181)
#define Flag182(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].V.EX.X.fw3.flag182)
#define Flag183(N)     (Nodes_Ptr[(N) - First_Node_Id + 3].V.EX.X.fw3.flag183)
