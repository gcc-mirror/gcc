// { dg-do assemble  }
// GROUPS passed operator-new
typedef struct {
	int a;
} AStruct;

void MakeBug() {
	AStruct *job;

	// This used to crash, it should now give error(s).
	job = new AStruct[];// { dg-error "" } .*

	job = new AStruct;
}

int main () {
	MakeBug();
}
