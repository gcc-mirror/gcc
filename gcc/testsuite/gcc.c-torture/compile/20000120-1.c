extern char letters[26+1];
char letter;
int letter_number;
char letters[] = "AbCdefghiJklmNopQrStuVwXyZ";

static void
pad_home1 ()
{
  letter = letters[letter_number =
		   letters[letter_number + 1] ? letter_number +
		   1 : 0];
}

