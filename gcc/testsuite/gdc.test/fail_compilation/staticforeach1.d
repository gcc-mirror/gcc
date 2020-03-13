/*
TEST_OUTPUT:
---
fail_compilation/staticforeach1.d(10): Error: must use labeled `break` within `static foreach`
---
*/
void main(){
	for(;;){
		static foreach(i;0..1){
			break;
		}
	}
}
