/* In 3.0, this test case (extracted from Bigloo) crashes the compiler in
   bb-reorder.c.  This is a regression from 2.95, already fixed in 3.1.

   Original bug report is c/5830 by Manuel Serrano <Manuel.Serrano@inria.fr>.
 */

typedef union scmobj {
  struct pair {
    union scmobj *car;
    union scmobj *cdr;
  } pair_t;
  struct vector {
    long header;
    int length;
    union scmobj *obj0;
  } vector_t;
} *obj_t;

extern obj_t create_vector (int);
extern obj_t make_pair (obj_t, obj_t);
extern long bgl_list_length (obj_t);
extern int BGl_equalzf3zf3zz__r4_equivalence_6_2z00 (obj_t, obj_t);
extern obj_t BGl_evcompilezd2lambdazd2zz__evcompilez00 (obj_t
							BgL_formalsz00_39,
							obj_t BgL_bodyz00_40,
							obj_t BgL_wherez00_41,
							obj_t
							BgL_namedzf3zf3_42,
							obj_t BgL_locz00_43);

obj_t
BGl_evcompilezd2lambdazd2zz__evcompilez00 (obj_t BgL_formalsz00_39,
					   obj_t BgL_bodyz00_40,
					   obj_t BgL_wherez00_41,
					   obj_t BgL_namedzf3zf3_42,
					   obj_t BgL_locz00_43)
{
  if (BGl_equalzf3zf3zz__r4_equivalence_6_2z00
      (BgL_formalsz00_39,
       ((obj_t) (obj_t) ((long) (((long) (0) << 2) | 2))))) {
  BgL_tagzd21966zd2_943:
    if ((BgL_namedzf3zf3_42 !=
	 ((obj_t) (obj_t) ((long) (((long) (1) << 2) | 2))))) {
      obj_t BgL_v1042z00_998;
      {
	int BgL_auxz00_4066;
	BgL_auxz00_4066 = (int) (((long) 3));
	BgL_v1042z00_998 = create_vector (BgL_auxz00_4066);
      }
      {
	obj_t BgL_arg1586z00_1000;
	BgL_arg1586z00_1000 = make_pair (BgL_wherez00_41, BgL_bodyz00_40);
	{
	  int BgL_auxz00_4070;
	  BgL_auxz00_4070 = (int) (((long) 2));
	  ((&(((obj_t) (BgL_v1042z00_998))->vector_t.obj0))[BgL_auxz00_4070] =
	   BgL_arg1586z00_1000,
	   ((obj_t) (obj_t) ((long) (((long) (3) << 2) | 2))));
	}
      }
      {
	int BgL_auxz00_4073;
	BgL_auxz00_4073 = (int) (((long) 1));
	((&(((obj_t) (BgL_v1042z00_998))->vector_t.obj0))[BgL_auxz00_4073] =
	 BgL_locz00_43, ((obj_t) (obj_t) ((long) (((long) (3) << 2) | 2))));
      }
      {
	obj_t BgL_auxz00_4078;
	int BgL_auxz00_4076;
	{
	  long BgL_auxz00_4079;
	  {
	    long BgL_auxz00_4080;
	    BgL_auxz00_4080 = bgl_list_length (BgL_formalsz00_39);
	    BgL_auxz00_4079 = (BgL_auxz00_4080 + ((long) 37));
	  }
	  BgL_auxz00_4078 =
	    (obj_t) ((long) (((long) (BgL_auxz00_4079) << 2) | 1));
	}
	BgL_auxz00_4076 = (int) (((long) 0));
	((&(((obj_t) (BgL_v1042z00_998))->vector_t.obj0))[BgL_auxz00_4076] =
	 BgL_auxz00_4078, ((obj_t) (obj_t) ((long) (((long) (3) << 2) | 2))));
      }
      return BgL_v1042z00_998;
    } else {
      obj_t BgL_v1043z00_1005;
      {
	int BgL_auxz00_4085;
	BgL_auxz00_4085 = (int) (((long) 3));
	BgL_v1043z00_1005 = create_vector (BgL_auxz00_4085);
      }
      {
	int BgL_auxz00_4088;
	BgL_auxz00_4088 = (int) (((long) 2));
	((&(((obj_t) (BgL_v1043z00_1005))->vector_t.obj0))[BgL_auxz00_4088] =
	 BgL_bodyz00_40, ((obj_t) (obj_t) ((long) (((long) (3) << 2) | 2))));
      }
      {
	int BgL_auxz00_4091;
	BgL_auxz00_4091 = (int) (((long) 1));
	((&(((obj_t) (BgL_v1043z00_1005))->vector_t.obj0))[BgL_auxz00_4091] =
	 BgL_locz00_43, ((obj_t) (obj_t) ((long) (((long) (3) << 2) | 2))));
      }
      {
	obj_t BgL_auxz00_4096;
	int BgL_auxz00_4094;
	{
	  long BgL_auxz00_4097;
	  {
	    long BgL_auxz00_4098;
	    BgL_auxz00_4098 = bgl_list_length (BgL_formalsz00_39);
	    BgL_auxz00_4097 = (BgL_auxz00_4098 + ((long) 42));
	  }
	  BgL_auxz00_4096 =
	    (obj_t) ((long) (((long) (BgL_auxz00_4097) << 2) | 1));
	}
	BgL_auxz00_4094 = (int) (((long) 0));
	((&(((obj_t) (BgL_v1043z00_1005))->vector_t.obj0))[BgL_auxz00_4094] =
	 BgL_auxz00_4096, ((obj_t) (obj_t) ((long) (((long) (3) << 2) | 2))));
      }
      return BgL_v1043z00_1005;
    }
  } else {
    if (((((long) BgL_formalsz00_39) & ((1 << 2) - 1)) == 3)) {
      if (BGl_equalzf3zf3zz__r4_equivalence_6_2z00
	  (((((obj_t) ((long) BgL_formalsz00_39 - 3))->pair_t).cdr),
	   ((obj_t) (obj_t) ((long) (((long) (0) << 2) | 2))))) {
	goto BgL_tagzd21966zd2_943;
      } else {
	obj_t BgL_cdrzd21979zd2_953;
	BgL_cdrzd21979zd2_953 =
	  ((((obj_t) ((long) BgL_formalsz00_39 - 3))->pair_t).cdr);
	if (((((long) BgL_cdrzd21979zd2_953) & ((1 << 2) - 1)) == 3)) {
	  if (BGl_equalzf3zf3zz__r4_equivalence_6_2z00
	      (((((obj_t) ((long) BgL_cdrzd21979zd2_953 - 3))->pair_t).cdr),
	       ((obj_t) (obj_t) ((long) (((long) (0) << 2) | 2))))) {
	    goto BgL_tagzd21966zd2_943;
	  } else {
	    obj_t BgL_cdrzd21986zd2_956;
	    BgL_cdrzd21986zd2_956 =
	      ((((obj_t) ((long) BgL_cdrzd21979zd2_953 - 3))->pair_t).cdr);
	    if (((((long) BgL_cdrzd21986zd2_956) & ((1 << 2) - 1)) == 3)) {
	      if (BGl_equalzf3zf3zz__r4_equivalence_6_2z00
		  (((((obj_t) ((long) BgL_cdrzd21986zd2_956 - 3))->pair_t).
		    cdr),
		   ((obj_t) (obj_t) ((long) (((long) (0) << 2) | 2))))) {
		goto BgL_tagzd21966zd2_943;
	      } else {
		obj_t BgL_cdrzd21994zd2_959;
		{
		  obj_t BgL_auxz00_4120;
		  BgL_auxz00_4120 =
		    ((((obj_t) ((long) BgL_cdrzd21979zd2_953 - 3))->pair_t).
		     cdr);
		  BgL_cdrzd21994zd2_959 =
		    ((((obj_t) ((long) BgL_auxz00_4120 - 3))->pair_t).cdr);
		}
		if (((((long) BgL_cdrzd21994zd2_959) & ((1 << 2) - 1)) == 3)) {
		  if (BGl_equalzf3zf3zz__r4_equivalence_6_2z00
		      (((((obj_t) ((long) BgL_cdrzd21994zd2_959 - 3))->
			 pair_t).cdr),
		       ((obj_t) (obj_t) ((long) (((long) (0) << 2) | 2))))) {
		    goto BgL_tagzd21966zd2_943;
		  } else {
		    int BgL_testz00_4128;
		    {
		      obj_t BgL_auxz00_4129;
		      BgL_auxz00_4129 =
			((((obj_t) ((long) BgL_formalsz00_39 - 3))->pair_t).
			 car);
		      BgL_testz00_4128 =
			((((long) BgL_auxz00_4129) & ((1 << 2) - 1)) == 3);
		    }
		    if (BgL_testz00_4128) {
		    BgL_tagzd21971zd2_948:
		      if ((BgL_namedzf3zf3_42 !=
			   ((obj_t) (obj_t)
			    ((long) (((long) (1) << 2) | 2))))) {
			obj_t BgL_v1052z00_1026;
			{
			  int BgL_auxz00_4134;
			  BgL_auxz00_4134 = (int) (((long) 3));
			  BgL_v1052z00_1026 = create_vector (BgL_auxz00_4134);
			}
			{
			  obj_t BgL_arg1606z00_1028;
			  {
			    obj_t BgL_v1053z00_1029;
			    {
			      int BgL_auxz00_4137;
			      BgL_auxz00_4137 = (int) (((long) 3));
			      BgL_v1053z00_1029 =
				create_vector (BgL_auxz00_4137);
			    }
			    {
			      int BgL_auxz00_4140;
			      BgL_auxz00_4140 = (int) (((long) 2));
			      ((&
				(((obj_t) (BgL_v1053z00_1029))->vector_t.
				 obj0))[BgL_auxz00_4140] =
			       BgL_formalsz00_39,
			       ((obj_t) (obj_t)
				((long) (((long) (3) << 2) | 2))));
			    }
			    {
			      int BgL_auxz00_4143;
			      BgL_auxz00_4143 = (int) (((long) 1));
			      ((&
				(((obj_t) (BgL_v1053z00_1029))->vector_t.
				 obj0))[BgL_auxz00_4143] =
			       BgL_bodyz00_40,
			       ((obj_t) (obj_t)
				((long) (((long) (3) << 2) | 2))));
			    }
			    {
			      int BgL_auxz00_4146;
			      BgL_auxz00_4146 = (int) (((long) 0));
			      ((&
				(((obj_t) (BgL_v1053z00_1029))->vector_t.
				 obj0))[BgL_auxz00_4146] =
			       BgL_wherez00_41,
			       ((obj_t) (obj_t)
				((long) (((long) (3) << 2) | 2))));
			    }
			    BgL_arg1606z00_1028 = BgL_v1053z00_1029;
			  }
			  {
			    int BgL_auxz00_4149;
			    BgL_auxz00_4149 = (int) (((long) 2));
			    ((&(((obj_t) (BgL_v1052z00_1026))->vector_t.obj0))
			     [BgL_auxz00_4149] =
			     BgL_arg1606z00_1028,
			     ((obj_t) (obj_t)
			      ((long) (((long) (3) << 2) | 2))));
			  }
			}
			{
			  int BgL_auxz00_4152;
			  BgL_auxz00_4152 = (int) (((long) 1));
			  ((&(((obj_t) (BgL_v1052z00_1026))->vector_t.obj0))
			   [BgL_auxz00_4152] =
			   BgL_locz00_43,
			   ((obj_t) (obj_t)
			    ((long) (((long) (3) << 2) | 2))));
			}
			{
			  obj_t BgL_auxz00_4157;
			  int BgL_auxz00_4155;
			  BgL_auxz00_4157 =
			    (obj_t) ((long)
				     (((long) (((long) 55)) << 2) | 1));
			  BgL_auxz00_4155 = (int) (((long) 0));
			  ((&(((obj_t) (BgL_v1052z00_1026))->vector_t.obj0))
			   [BgL_auxz00_4155] =
			   BgL_auxz00_4157,
			   ((obj_t) (obj_t)
			    ((long) (((long) (3) << 2) | 2))));
			}
			return BgL_v1052z00_1026;
		      } else {
			obj_t BgL_v1054z00_1030;
			{
			  int BgL_auxz00_4160;
			  BgL_auxz00_4160 = (int) (((long) 3));
			  BgL_v1054z00_1030 = create_vector (BgL_auxz00_4160);
			}
			{
			  obj_t BgL_arg1608z00_1032;
			  BgL_arg1608z00_1032 =
			    make_pair (BgL_bodyz00_40, BgL_formalsz00_39);
			  {
			    int BgL_auxz00_4164;
			    BgL_auxz00_4164 = (int) (((long) 2));
			    ((&(((obj_t) (BgL_v1054z00_1030))->vector_t.obj0))
			     [BgL_auxz00_4164] =
			     BgL_arg1608z00_1032,
			     ((obj_t) (obj_t)
			      ((long) (((long) (3) << 2) | 2))));
			  }
			}
			{
			  int BgL_auxz00_4167;
			  BgL_auxz00_4167 = (int) (((long) 1));
			  ((&(((obj_t) (BgL_v1054z00_1030))->vector_t.obj0))
			   [BgL_auxz00_4167] =
			   BgL_locz00_43,
			   ((obj_t) (obj_t)
			    ((long) (((long) (3) << 2) | 2))));
			}
			{
			  obj_t BgL_auxz00_4172;
			  int BgL_auxz00_4170;
			  BgL_auxz00_4172 =
			    (obj_t) ((long)
				     (((long) (((long) 56)) << 2) | 1));
			  BgL_auxz00_4170 = (int) (((long) 0));
			  ((&(((obj_t) (BgL_v1054z00_1030))->vector_t.obj0))
			   [BgL_auxz00_4170] =
			   BgL_auxz00_4172,
			   ((obj_t) (obj_t)
			    ((long) (((long) (3) << 2) | 2))));
			}
			return BgL_v1054z00_1030;
		      }
		    } else {
		      int BgL_testz00_4175;
		      {
			obj_t BgL_auxz00_4176;
			{
			  obj_t BgL_auxz00_4177;
			  BgL_auxz00_4177 =
			    ((((obj_t) ((long) BgL_formalsz00_39 - 3))->
			      pair_t).cdr);
			  BgL_auxz00_4176 =
			    ((((obj_t) ((long) BgL_auxz00_4177 - 3))->pair_t).
			     car);
			}
			BgL_testz00_4175 =
			  ((((long) BgL_auxz00_4176) & ((1 << 2) - 1)) == 3);
		      }
		      if (BgL_testz00_4175) {
			goto BgL_tagzd21971zd2_948;
		      } else {
			int BgL_testz00_4181;
			{
			  obj_t BgL_auxz00_4182;
			  {
			    obj_t BgL_auxz00_4183;
			    {
			      obj_t BgL_auxz00_4184;
			      BgL_auxz00_4184 =
				((((obj_t) ((long) BgL_formalsz00_39 - 3))->
				  pair_t).cdr);
			      BgL_auxz00_4183 =
				((((obj_t) ((long) BgL_auxz00_4184 - 3))->
				  pair_t).cdr);
			    }
			    BgL_auxz00_4182 =
			      ((((obj_t) ((long) BgL_auxz00_4183 - 3))->
				pair_t).car);
			  }
			  BgL_testz00_4181 =
			    ((((long) BgL_auxz00_4182) & ((1 << 2) - 1)) ==
			     3);
			}
			if (BgL_testz00_4181) {
			  goto BgL_tagzd21971zd2_948;
			} else {
			  goto BgL_tagzd21971zd2_948;
			}
		      }
		    }
		  }
		} else {
		  int BgL_testz00_4189;
		  {
		    obj_t BgL_auxz00_4190;
		    BgL_auxz00_4190 =
		      ((((obj_t) ((long) BgL_formalsz00_39 - 3))->pair_t).
		       car);
		    BgL_testz00_4189 =
		      ((((long) BgL_auxz00_4190) & ((1 << 2) - 1)) == 3);
		  }
		  if (BgL_testz00_4189) {
		    goto BgL_tagzd21971zd2_948;
		  } else {
		    int BgL_testz00_4193;
		    {
		      obj_t BgL_auxz00_4194;
		      {
			obj_t BgL_auxz00_4195;
			BgL_auxz00_4195 =
			  ((((obj_t) ((long) BgL_formalsz00_39 - 3))->pair_t).
			   cdr);
			BgL_auxz00_4194 =
			  ((((obj_t) ((long) BgL_auxz00_4195 - 3))->pair_t).
			   car);
		      }
		      BgL_testz00_4193 =
			((((long) BgL_auxz00_4194) & ((1 << 2) - 1)) == 3);
		    }
		    if (BgL_testz00_4193) {
		      goto BgL_tagzd21971zd2_948;
		    } else {
		      int BgL_testz00_4199;
		      {
			obj_t BgL_auxz00_4200;
			{
			  obj_t BgL_auxz00_4201;
			  {
			    obj_t BgL_auxz00_4202;
			    BgL_auxz00_4202 =
			      ((((obj_t) ((long) BgL_formalsz00_39 - 3))->
				pair_t).cdr);
			    BgL_auxz00_4201 =
			      ((((obj_t) ((long) BgL_auxz00_4202 - 3))->
				pair_t).cdr);
			  }
			  BgL_auxz00_4200 =
			    ((((obj_t) ((long) BgL_auxz00_4201 - 3))->pair_t).
			     car);
			}
			BgL_testz00_4199 =
			  ((((long) BgL_auxz00_4200) & ((1 << 2) - 1)) == 3);
		      }
		      if (BgL_testz00_4199) {
			goto BgL_tagzd21971zd2_948;
		      } else {
			if ((BgL_namedzf3zf3_42 !=
			     ((obj_t) (obj_t)
			      ((long) (((long) (1) << 2) | 2))))) {
			  obj_t BgL_v1050z00_1022;
			  {
			    int BgL_auxz00_4209;
			    BgL_auxz00_4209 = (int) (((long) 3));
			    BgL_v1050z00_1022 =
			      create_vector (BgL_auxz00_4209);
			  }
			  {
			    obj_t BgL_arg1604z00_1024;
			    BgL_arg1604z00_1024 =
			      make_pair (BgL_wherez00_41, BgL_bodyz00_40);
			    {
			      int BgL_auxz00_4213;
			      BgL_auxz00_4213 = (int) (((long) 2));
			      ((&
				(((obj_t) (BgL_v1050z00_1022))->vector_t.
				 obj0))[BgL_auxz00_4213] =
			       BgL_arg1604z00_1024,
			       ((obj_t) (obj_t)
				((long) (((long) (3) << 2) | 2))));
			    }
			  }
			  {
			    int BgL_auxz00_4216;
			    BgL_auxz00_4216 = (int) (((long) 1));
			    ((&(((obj_t) (BgL_v1050z00_1022))->vector_t.obj0))
			     [BgL_auxz00_4216] =
			     BgL_locz00_43,
			     ((obj_t) (obj_t)
			      ((long) (((long) (3) << 2) | 2))));
			  }
			  {
			    obj_t BgL_auxz00_4221;
			    int BgL_auxz00_4219;
			    BgL_auxz00_4221 =
			      (obj_t) ((long)
				       (((long) (((long) 50)) << 2) | 1));
			    BgL_auxz00_4219 = (int) (((long) 0));
			    ((&(((obj_t) (BgL_v1050z00_1022))->vector_t.obj0))
			     [BgL_auxz00_4219] =
			     BgL_auxz00_4221,
			     ((obj_t) (obj_t)
			      ((long) (((long) (3) << 2) | 2))));
			  }
			  return BgL_v1050z00_1022;
			} else {
			  obj_t BgL_v1051z00_1025;
			  {
			    int BgL_auxz00_4224;
			    BgL_auxz00_4224 = (int) (((long) 3));
			    BgL_v1051z00_1025 =
			      create_vector (BgL_auxz00_4224);
			  }
			  {
			    int BgL_auxz00_4227;
			    BgL_auxz00_4227 = (int) (((long) 2));
			    ((&(((obj_t) (BgL_v1051z00_1025))->vector_t.obj0))
			     [BgL_auxz00_4227] =
			     BgL_bodyz00_40,
			     ((obj_t) (obj_t)
			      ((long) (((long) (3) << 2) | 2))));
			  }
			  {
			    int BgL_auxz00_4230;
			    BgL_auxz00_4230 = (int) (((long) 1));
			    ((&(((obj_t) (BgL_v1051z00_1025))->vector_t.obj0))
			     [BgL_auxz00_4230] =
			     BgL_locz00_43,
			     ((obj_t) (obj_t)
			      ((long) (((long) (3) << 2) | 2))));
			  }
			  {
			    obj_t BgL_auxz00_4235;
			    int BgL_auxz00_4233;
			    BgL_auxz00_4235 =
			      (obj_t) ((long)
				       (((long) (((long) 54)) << 2) | 1));
			    BgL_auxz00_4233 = (int) (((long) 0));
			    ((&(((obj_t) (BgL_v1051z00_1025))->vector_t.obj0))
			     [BgL_auxz00_4233] =
			     BgL_auxz00_4235,
			     ((obj_t) (obj_t)
			      ((long) (((long) (3) << 2) | 2))));
			  }
			  return BgL_v1051z00_1025;
			}
		      }
		    }
		  }
		}
	      }
	    } else {
	      int BgL_testz00_4238;
	      {
		obj_t BgL_auxz00_4239;
		BgL_auxz00_4239 =
		  ((((obj_t) ((long) BgL_formalsz00_39 - 3))->pair_t).car);
		BgL_testz00_4238 =
		  ((((long) BgL_auxz00_4239) & ((1 << 2) - 1)) == 3);
	      }
	      if (BgL_testz00_4238) {
		goto BgL_tagzd21971zd2_948;
	      } else {
		int BgL_testz00_4242;
		{
		  obj_t BgL_auxz00_4243;
		  BgL_auxz00_4243 =
		    ((((obj_t) ((long) BgL_cdrzd21979zd2_953 - 3))->pair_t).
		     car);
		  BgL_testz00_4242 =
		    ((((long) BgL_auxz00_4243) & ((1 << 2) - 1)) == 3);
		}
		if (BgL_testz00_4242) {
		  goto BgL_tagzd21971zd2_948;
		} else {
		  if ((BgL_namedzf3zf3_42 !=
		       ((obj_t) (obj_t) ((long) (((long) (1) << 2) | 2))))) {
		    obj_t BgL_v1048z00_1018;
		    {
		      int BgL_auxz00_4248;
		      BgL_auxz00_4248 = (int) (((long) 3));
		      BgL_v1048z00_1018 = create_vector (BgL_auxz00_4248);
		    }
		    {
		      obj_t BgL_arg1602z00_1020;
		      BgL_arg1602z00_1020 =
			make_pair (BgL_wherez00_41, BgL_bodyz00_40);
		      {
			int BgL_auxz00_4252;
			BgL_auxz00_4252 = (int) (((long) 2));
			((&(((obj_t) (BgL_v1048z00_1018))->vector_t.obj0))
			 [BgL_auxz00_4252] =
			 BgL_arg1602z00_1020,
			 ((obj_t) (obj_t) ((long) (((long) (3) << 2) | 2))));
		      }
		    }
		    {
		      int BgL_auxz00_4255;
		      BgL_auxz00_4255 = (int) (((long) 1));
		      ((&(((obj_t) (BgL_v1048z00_1018))->vector_t.obj0))
		       [BgL_auxz00_4255] =
		       BgL_locz00_43,
		       ((obj_t) (obj_t) ((long) (((long) (3) << 2) | 2))));
		    }
		    {
		      obj_t BgL_auxz00_4260;
		      int BgL_auxz00_4258;
		      BgL_auxz00_4260 =
			(obj_t) ((long) (((long) (((long) 49)) << 2) | 1));
		      BgL_auxz00_4258 = (int) (((long) 0));
		      ((&(((obj_t) (BgL_v1048z00_1018))->vector_t.obj0))
		       [BgL_auxz00_4258] =
		       BgL_auxz00_4260,
		       ((obj_t) (obj_t) ((long) (((long) (3) << 2) | 2))));
		    }
		    return BgL_v1048z00_1018;
		  } else {
		    obj_t BgL_v1049z00_1021;
		    {
		      int BgL_auxz00_4263;
		      BgL_auxz00_4263 = (int) (((long) 3));
		      BgL_v1049z00_1021 = create_vector (BgL_auxz00_4263);
		    }
		    {
		      int BgL_auxz00_4266;
		      BgL_auxz00_4266 = (int) (((long) 2));
		      ((&(((obj_t) (BgL_v1049z00_1021))->vector_t.obj0))
		       [BgL_auxz00_4266] =
		       BgL_bodyz00_40,
		       ((obj_t) (obj_t) ((long) (((long) (3) << 2) | 2))));
		    }
		    {
		      int BgL_auxz00_4269;
		      BgL_auxz00_4269 = (int) (((long) 1));
		      ((&(((obj_t) (BgL_v1049z00_1021))->vector_t.obj0))
		       [BgL_auxz00_4269] =
		       BgL_locz00_43,
		       ((obj_t) (obj_t) ((long) (((long) (3) << 2) | 2))));
		    }
		    {
		      obj_t BgL_auxz00_4274;
		      int BgL_auxz00_4272;
		      BgL_auxz00_4274 =
			(obj_t) ((long) (((long) (((long) 53)) << 2) | 1));
		      BgL_auxz00_4272 = (int) (((long) 0));
		      ((&(((obj_t) (BgL_v1049z00_1021))->vector_t.obj0))
		       [BgL_auxz00_4272] =
		       BgL_auxz00_4274,
		       ((obj_t) (obj_t) ((long) (((long) (3) << 2) | 2))));
		    }
		    return BgL_v1049z00_1021;
		  }
		}
	      }
	    }
	  }
	} else {
	  int BgL_testz00_4277;
	  {
	    obj_t BgL_auxz00_4278;
	    BgL_auxz00_4278 =
	      ((((obj_t) ((long) BgL_formalsz00_39 - 3))->pair_t).car);
	    BgL_testz00_4277 =
	      ((((long) BgL_auxz00_4278) & ((1 << 2) - 1)) == 3);
	  }
	  if (BgL_testz00_4277) {
	    goto BgL_tagzd21971zd2_948;
	  } else {
	    if ((BgL_namedzf3zf3_42 !=
		 ((obj_t) (obj_t) ((long) (((long) (1) << 2) | 2))))) {
	      obj_t BgL_v1046z00_1014;
	      {
		int BgL_auxz00_4283;
		BgL_auxz00_4283 = (int) (((long) 3));
		BgL_v1046z00_1014 = create_vector (BgL_auxz00_4283);
	      }
	      {
		obj_t BgL_arg1600z00_1016;
		BgL_arg1600z00_1016 =
		  make_pair (BgL_wherez00_41, BgL_bodyz00_40);
		{
		  int BgL_auxz00_4287;
		  BgL_auxz00_4287 = (int) (((long) 2));
		  ((&(((obj_t) (BgL_v1046z00_1014))->vector_t.obj0))
		   [BgL_auxz00_4287] =
		   BgL_arg1600z00_1016,
		   ((obj_t) (obj_t) ((long) (((long) (3) << 2) | 2))));
		}
	      }
	      {
		int BgL_auxz00_4290;
		BgL_auxz00_4290 = (int) (((long) 1));
		((&(((obj_t) (BgL_v1046z00_1014))->vector_t.obj0))
		 [BgL_auxz00_4290] =
		 BgL_locz00_43,
		 ((obj_t) (obj_t) ((long) (((long) (3) << 2) | 2))));
	      }
	      {
		obj_t BgL_auxz00_4295;
		int BgL_auxz00_4293;
		BgL_auxz00_4295 =
		  (obj_t) ((long) (((long) (((long) 48)) << 2) | 1));
		BgL_auxz00_4293 = (int) (((long) 0));
		((&(((obj_t) (BgL_v1046z00_1014))->vector_t.obj0))
		 [BgL_auxz00_4293] =
		 BgL_auxz00_4295,
		 ((obj_t) (obj_t) ((long) (((long) (3) << 2) | 2))));
	      }
	      return BgL_v1046z00_1014;
	    } else {
	      obj_t BgL_v1047z00_1017;
	      {
		int BgL_auxz00_4298;
		BgL_auxz00_4298 = (int) (((long) 3));
		BgL_v1047z00_1017 = create_vector (BgL_auxz00_4298);
	      }
	      {
		int BgL_auxz00_4301;
		BgL_auxz00_4301 = (int) (((long) 2));
		((&(((obj_t) (BgL_v1047z00_1017))->vector_t.obj0))
		 [BgL_auxz00_4301] =
		 BgL_bodyz00_40,
		 ((obj_t) (obj_t) ((long) (((long) (3) << 2) | 2))));
	      }
	      {
		int BgL_auxz00_4304;
		BgL_auxz00_4304 = (int) (((long) 1));
		((&(((obj_t) (BgL_v1047z00_1017))->vector_t.obj0))
		 [BgL_auxz00_4304] =
		 BgL_locz00_43,
		 ((obj_t) (obj_t) ((long) (((long) (3) << 2) | 2))));
	      }
	      {
		obj_t BgL_auxz00_4309;
		int BgL_auxz00_4307;
		BgL_auxz00_4309 =
		  (obj_t) ((long) (((long) (((long) 52)) << 2) | 1));
		BgL_auxz00_4307 = (int) (((long) 0));
		((&(((obj_t) (BgL_v1047z00_1017))->vector_t.obj0))
		 [BgL_auxz00_4307] =
		 BgL_auxz00_4309,
		 ((obj_t) (obj_t) ((long) (((long) (3) << 2) | 2))));
	      }
	      return BgL_v1047z00_1017;
	    }
	  }
	}
      }
    } else {
      if ((BgL_namedzf3zf3_42 !=
	   ((obj_t) (obj_t) ((long) (((long) (1) << 2) | 2))))) {
	obj_t BgL_v1044z00_1010;
	{
	  int BgL_auxz00_4314;
	  BgL_auxz00_4314 = (int) (((long) 3));
	  BgL_v1044z00_1010 = create_vector (BgL_auxz00_4314);
	}
	{
	  obj_t BgL_arg1598z00_1012;
	  BgL_arg1598z00_1012 = make_pair (BgL_wherez00_41, BgL_bodyz00_40);
	  {
	    int BgL_auxz00_4318;
	    BgL_auxz00_4318 = (int) (((long) 2));
	    ((&(((obj_t) (BgL_v1044z00_1010))->vector_t.obj0))
	     [BgL_auxz00_4318] =
	     BgL_arg1598z00_1012,
	     ((obj_t) (obj_t) ((long) (((long) (3) << 2) | 2))));
	  }
	}
	{
	  int BgL_auxz00_4321;
	  BgL_auxz00_4321 = (int) (((long) 1));
	  ((&(((obj_t) (BgL_v1044z00_1010))->vector_t.obj0))[BgL_auxz00_4321]
	   =
	   BgL_locz00_43, ((obj_t) (obj_t) ((long) (((long) (3) << 2) | 2))));
	}
	{
	  obj_t BgL_auxz00_4326;
	  int BgL_auxz00_4324;
	  BgL_auxz00_4326 =
	    (obj_t) ((long) (((long) (((long) 47)) << 2) | 1));
	  BgL_auxz00_4324 = (int) (((long) 0));
	  ((&(((obj_t) (BgL_v1044z00_1010))->vector_t.obj0))[BgL_auxz00_4324]
	   =
	   BgL_auxz00_4326,
	   ((obj_t) (obj_t) ((long) (((long) (3) << 2) | 2))));
	}
	return BgL_v1044z00_1010;
      } else {
	obj_t BgL_v1045z00_1013;
	{
	  int BgL_auxz00_4329;
	  BgL_auxz00_4329 = (int) (((long) 3));
	  BgL_v1045z00_1013 = create_vector (BgL_auxz00_4329);
	}
	{
	  int BgL_auxz00_4332;
	  BgL_auxz00_4332 = (int) (((long) 2));
	  ((&(((obj_t) (BgL_v1045z00_1013))->vector_t.obj0))[BgL_auxz00_4332]
	   =
	   BgL_bodyz00_40,
	   ((obj_t) (obj_t) ((long) (((long) (3) << 2) | 2))));
	}
	{
	  int BgL_auxz00_4335;
	  BgL_auxz00_4335 = (int) (((long) 1));
	  ((&(((obj_t) (BgL_v1045z00_1013))->vector_t.obj0))[BgL_auxz00_4335]
	   =
	   BgL_locz00_43, ((obj_t) (obj_t) ((long) (((long) (3) << 2) | 2))));
	}
	{
	  obj_t BgL_auxz00_4340;
	  int BgL_auxz00_4338;
	  BgL_auxz00_4340 =
	    (obj_t) ((long) (((long) (((long) 51)) << 2) | 1));
	  BgL_auxz00_4338 = (int) (((long) 0));
	  ((&(((obj_t) (BgL_v1045z00_1013))->vector_t.obj0))[BgL_auxz00_4338]
	   =
	   BgL_auxz00_4340,
	   ((obj_t) (obj_t) ((long) (((long) (3) << 2) | 2))));
	}
	return BgL_v1045z00_1013;
      }
    }
  }
}
