/* N3355 - Named loops.  */
/* { dg-do compile } */
/* { dg-options "-std=gnu2y" } */

void
foo (int x)
{
  for (int i = 0; i < 16; ++i)
    {
      int k;
     label1:									/* { dg-message "loop name defined here" } */
      for (int j = ({ if (x == 0) break label1; 0; }); j < 16; ++j)		/* { dg-error "'break' statement operand 'label1' refers to a loop outside of its body" } */
	;
     label2:									/* { dg-message "loop name defined here" } */
      for (int j = ({ if (x == 1) continue label2; 0; }); j < 16; ++j)		/* { dg-error "'continue' statement operand 'label2' refers to a loop outside of its body" } */
	;
     label3:									/* { dg-message "loop name defined here" } */
      for (int j = 0; j < ({ if (x == 2) break label3; 16; }); ++j)		/* { dg-error "'break' statement operand 'label3' refers to a loop outside of its body" } */
	;
     label4:									/* { dg-message "loop name defined here" } */
      for (int j = 0; j < ({ if (x == 3) continue label4; 16; }); ++j)		/* { dg-error "'continue' statement operand 'label4' refers to a loop outside of its body" } */
	;
     label5:									/* { dg-message "loop name defined here" } */
      for (int j = 0; j < 16; j += ({ if (x == 4) break label5; 1; }))		/* { dg-error "'break' statement operand 'label5' refers to a loop outside of its body" } */
	;
     label6:									/* { dg-message "loop name defined here" } */
      for (int j = 0; j < 16; j += ({ if (x == 5) continue label6; 1; }))	/* { dg-error "'continue' statement operand 'label6' refers to a loop outside of its body" } */
	;
      k = 0;
     label7:									/* { dg-message "loop name defined here" } */
      while (k < ({ if (x == 6) break label7; 16; }))				/* { dg-error "'break' statement operand 'label7' refers to a loop outside of its body" } */
	++k;
      k = 0;
     label8:									/* { dg-message "loop name defined here" } */
      while (k < ({ if (x == 7) continue label8; 16; }))			/* { dg-error "'continue' statement operand 'label8' refers to a loop outside of its body" } */
	++k;
      k = 0;
     label9:
      do
	++k;
      while (k <= ({ if (x == 8) break label9; 16; }));				/* { dg-error "'break' statement operand 'label9' does not refer to a named loop or 'switch'" } */
      k = 0;
     label10:
      do
	++k;
      while (k <= ({ if (x == 9) continue label10; 16; }));			/* { dg-error "'continue' statement operand 'label10' does not refer to a named loop" } */
     label11:									/* { dg-message "'switch' name defined here" } */
      switch (x + ({ if (x == 10) break label11; 0; }))				/* { dg-error "'break' statement operand 'label11' refers to a 'switch' outside of its body" } */
	{
	case 0:
	  break;
	}
    }
 label12:
 label13:
 label14:
  for (int i = 0; i < 32; ++i)
    {
     label15:
      switch (i)
	{
	 label16:
	case 0:
	 label17:
	 label18:
	 label19:
	 label20:
	 label21:
	 label22:
	 label23:
	 label24:
	 label25:
	 label26:
	 label27:
	 label28:
	 label29:
	 label30:
	  for (int j = 0; j < 32; ++j)
	    {
	      if (j == 31)
		continue label14;
	      else if (j == 30)
		break label15;
	      void bar (void)
	      {
	       label31:
		for (int k = 0; k < 32; ++k)
		  if (k == 31)
		    continue label31;
		  else if (k == 30)
		    break label31;
		  else if (k == 29)
		    continue label22;						/* { dg-error "'continue' statement operand 'label22' does not refer to a named loop; did you mean 'label31'\\\?" } */
		  else if (k == 28)
		    break label20;						/* { dg-error "'break' statement operand 'label20' does not refer to a named loop or 'switch'; did you mean 'label31'\\\?" } */
		  else if (k == 27)
		    break label15;						/* { dg-error "'break' statement operand 'label15' does not refer to a named loop or 'switch'; did you mean 'label31'\\\?" } */
		  else if (k == 26)
		    continue label13;						/* { dg-error "'continue' statement operand 'label13' does not refer to a named loop; did you mean 'label31'\\\?" } */
		  else if (k == 25)
		    break label12;						/* { dg-error "'break' statement operand 'label12' does not refer to a named loop or 'switch'; did you mean 'label31'\\\?" } */
	      }
	      bar ();
	      if (j == 29)
		continue label22;
	      else if (j == 28)
		break label20;
	      else if (j == 27)
		break label15;
	      else if (j == 26)
		continue label13;
	      else if (j == 25)
		break label12;
	    }
	}
    }
}
