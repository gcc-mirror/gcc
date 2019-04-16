/result for low patch/	{
				sub(".*low patch ","")
				sub(" is as expected","")
				printf ("<-- %4s\n", $0);
				next
			}
/result for high patch/	{
				sub(".*high patch ","")
				sub(" is as expected","")
				printf ("    %4s -->\n", $0);
				next
			}
/patches later/		{
				sub(".*later than ","")
				printf ("<-- %4s\n", $0);
				next
			}
/patches earlier/	{
				sub(".*earlier than ","")
				printf ("    %4s -->\n", $0);
				next
			}
/build failed for/	{
				sub(".*build failed for ","")
				printf ("   [%4s]\n", $0);
				next
			}
/HIGH_PATCH/		{
				printf ("* stopped early *\n")
				next
			}
/changes with/		{
				sub(".*changes with id ","")
				printf ("+----------+\n")
				printf ("|   %4s   |\n", $0)
				printf ("+----------+\n")
				next
			}
