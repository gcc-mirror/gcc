	int i, ndec;
	uiolen n;
	{	(void) fseek(b->ufd,-(long)sizeof(uiolen),SEEK_CUR);
		(void) fread((char *)&n,sizeof(uiolen),1,b->ufd);
		(void) fseek(b->ufd,-(long)n-2*sizeof(uiolen),SEEK_CUR);
