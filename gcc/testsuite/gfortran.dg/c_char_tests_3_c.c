void test_char (char, char, char, char, char,
		char, char, char, char, char,
		char, char, char, char, char);

void test_int (char, char, char, char, char,
	       char, char, char, char, char,
	       char, char, char, char, char);

int main (void) {
  test_char ('a', 'b', 'c', 'd', 'e',
	     'f', 'g', 'h', 'i', 'j',
	     'k', 'l', 'm', 'n', 'o');
  test_int ('a', 'b', 'c', 'd', 'e',
	    'f', 'g', 'h', 'i', 'j',
	    'k', 'l', 'm', 'n', 'o');
}
