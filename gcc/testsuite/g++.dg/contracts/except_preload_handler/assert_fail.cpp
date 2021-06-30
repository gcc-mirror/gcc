#include <iostream>
#include <contract>

int fun() {
	int x = 0;
	[[ assert: x < 0 ]];

	return 0;
}

int main(int argc, char**) {
	try {
		fun();
	} catch(int &ex) {
		std::cerr << "synth caught indirect: " << ex << std::endl;
	}

	return 0;
}

