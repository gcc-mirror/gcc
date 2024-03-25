int m[8],b[8];
int rand(void);

int main(){
	int i;

	for(;;){
		m[0] = rand();
		if(m[0] == 0){
			for(i=0;i<8;i++){
				m[i] = b[i];
			}
			break;
		}
	}
}


