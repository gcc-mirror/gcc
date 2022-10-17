// Switch with no braces & empty case should compile

int main() {
	int ob = 1;

    final switch (ob)
    case 0: case 1:
        break;

    return ob;
}
