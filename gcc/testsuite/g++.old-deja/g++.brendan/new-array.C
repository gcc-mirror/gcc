// Build don't link: 
// GROUPS passed operator-new
typedef struct {
	int a;
} AStruct;

void MakeBug() {
	AStruct *job;

	// This used to crash, it should now give error(s).
	job = new AStruct[];// ERROR - .*

	job = new AStruct;
}

int main () {
	MakeBug();
}
