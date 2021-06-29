#include <iostream>

int main(int, char**) {
	int x = 0;
	[[ assert: x < 0 ]];

	std::cout << "returning from main" << std::endl;
	return 0;
}

